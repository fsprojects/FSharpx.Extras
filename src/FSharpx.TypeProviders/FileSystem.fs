module FSharpx.TypeProviders.FilesTypeProvider

// originally from https://gist.github.com/1241061

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open System.IO

let rec annotateWithSubdirectories (fileSystemInfo:FileSystemInfo) (ownerTy:ProvidedTypeDefinition) =
    try        
        let annotated =
            ownerTy.AddXmlDoc <| sprintf "A strongly typed interface to '%s'" fileSystemInfo.FullName
            ownerTy
                |> hideOldMethods
                |> addMember (literalField "Path" (sprintf "Full path to '%s'" fileSystemInfo.FullName) fileSystemInfo.FullName)
        
        match fileSystemInfo with
        | :? DirectoryInfo as dir ->
             dir.EnumerateFileSystemInfos()
                |> Seq.map (fun info -> runtimeType<obj> info.Name |> annotateWithSubdirectories info)
                |> Seq.fold (fun ownerType subdirType -> addMember subdirType ownerType) annotated
        | _ -> annotated    
    with 
    | exn -> ownerTy

let typedFileSystem =
    erasedType<obj> thisAssembly rootNamespace "FileSystemTyped"
      |> staticParameter "path" (fun typeName path -> 
            erasedType<obj> thisAssembly rootNamespace typeName
                |> annotateWithSubdirectories (new DirectoryInfo(path)))