module FSharpx.TypeProviders.FilesTypeProvider

// originally from https://gist.github.com/1241061

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL

let rec addMembers (path:string) (ownerTy:ProvidedTypeDefinition) =          
    ownerTy.AddXmlDoc "A strongly typed interface to the directory '%s'"
    let dir = new System.IO.DirectoryInfo(path)
    ownerTy 
        |> addMember (literalField "Path" "Full path to fullName" dir.FullName) 
        |> ignore
    for sub in dir.EnumerateDirectories() do
                let subTy = 
                    runtimeType<obj> (sub.Name.Replace(' ','_'))
                        |> hideOldMethods
                        |> addMember (literalField "Path" "Full path to fullName" sub.FullName)
                addMembersSafe sub.FullName subTy
                ownerTy.AddMember subTy
    for file in dir.EnumerateFiles() do
                let subTy = 
                    runtimeType<obj> (file.Name.Replace(' ','_'))
                        |> hideOldMethods
                        |> addMember (literalField "Path" "Full path to fullName" file.FullName)
                ownerTy.AddMember subTy
and addMembersSafe (path:string) (ownerTy:ProvidedTypeDefinition) = 
        try 
            addMembers path ownerTy
        with 
        | exn -> ()


let fileTy = erasedType<obj> thisAssembly rootNamespace "FileSystemTyped"

fileTy.DefineStaticParameters(
    parameters=[ProvidedStaticParameter("path", typeof<string>)], 
    instantiationFunction=(fun typeName parameterValues ->

        match parameterValues with 
        | [| :? string as path |] -> 
        let ty = erasedType<obj> thisAssembly rootNamespace typeName |> hideOldMethods
                    
        addMembersSafe path ty
                    
        ty
        | _ -> failwith "unexpected parameter values")) 