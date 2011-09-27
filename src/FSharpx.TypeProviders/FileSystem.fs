module FSharpx.TypeProviders.FilesTypeProvider

// originally from https://gist.github.com/1241061

open Samples.FSharpPreviewRelease2011.ProvidedTypes
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open System.IO

let rec annotateAsFileSystemInfo (fileSystemInfo:FileSystemInfo) (ownerTy:ProvidedTypeDefinition) =
    try        
        let annotated =
            ownerTy
                |> addXmlDoc (sprintf "A strongly typed interface to '%s'" fileSystemInfo.FullName)
                |> hideOldMethods
                |> addMember (
                    literalField "Path" fileSystemInfo.FullName
                      |> addXmlDoc (sprintf "Full path to '%s'" fileSystemInfo.FullName))
        
        match fileSystemInfo with
        | :? DirectoryInfo as dir ->
             dir.EnumerateFileSystemInfos()
                |> Seq.map (fun info -> runtimeType<obj> info.Name |> annotateAsFileSystemInfo info)
                |> Seq.fold (fun ownerType subdirType -> addMember subdirType ownerType) annotated
        | _ -> annotated    
    with 
    | exn -> ownerTy

let typedFileSystem =
    erasedType<obj> thisAssembly rootNamespace "FileSystemTyped"
      |> staticParameter "path" (fun typeName path -> 
            erasedType<obj> thisAssembly rootNamespace typeName
                |> annotateAsFileSystemInfo (new DirectoryInfo(path)))