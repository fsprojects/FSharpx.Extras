module FSharpx.TypeProviders.FilesTypeProvider

// originally from https://gist.github.com/1241061

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL

let rec addMembers (path:string) (ownerTy:ProvidedTypeDefinition) =
    try
        ownerTy.AddXmlDoc "A strongly typed interface to the directory '%s'"
        let dir = new System.IO.DirectoryInfo(path)
        let typeWithPath = ownerTy |> addMember (literalField "Path" "Full path to fullName" dir.FullName) 

        let typeWithSubdirectories =
            dir.EnumerateDirectories()
              |> Seq.map (fun sub -> sub,runtimeType<obj> sub.Name |> hideOldMethods)
              |> Seq.fold (fun ownerType (sub,dirType) ->                
                    addMember (addMembers sub.FullName dirType) ownerType)
                    typeWithPath

        dir.EnumerateFiles()
          |> Seq.map (fun file -> 
                runtimeType<obj> file.Name
                    |> hideOldMethods
                    |> addMember (literalField "Path" "Full path to fullName" file.FullName))
          |> Seq.fold (fun ownerType fileType -> addMember fileType ownerType) typeWithSubdirectories
    
    with 
    | exn -> ownerTy


let fileTy = erasedType<obj> thisAssembly rootNamespace "FileSystemTyped"

fileTy.DefineStaticParameters(
    parameters=[ProvidedStaticParameter("path", typeof<string>)], 
    instantiationFunction=(fun typeName parameterValues ->

        match parameterValues with 
        | [| :? string as path |] -> 
            erasedType<obj> thisAssembly rootNamespace typeName 
                |> hideOldMethods
                |> addMembers path
        | _ -> failwith "unexpected parameter values")) 