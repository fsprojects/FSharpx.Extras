module FSharpx.TypeProviders.FilesTypeProvider

// originally from https://gist.github.com/1241061

open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open System.IO

let rec annotateAsFileSystemInfo (fileSystemInfo:FileSystemInfo) (ownerTy:ProvidedTypeDefinition) () =
    try        
        let annotated =
            ownerTy
                |> addXmlDoc (sprintf "A strongly typed interface to '%s'" fileSystemInfo.FullName)
                |> hideOldMethods
                |+> (fun () ->
                        literalField "Path" fileSystemInfo.FullName
                         |> addXmlDoc (sprintf "Full path to '%s'" fileSystemInfo.FullName))
        
        match fileSystemInfo with
        | :? DirectoryInfo as dir ->
             annotated
               |++> (
                    dir.EnumerateFileSystemInfos()
                        |> Seq.map (fun info -> 
                                runtimeType<obj> info.Name 
                                    |> annotateAsFileSystemInfo info))
        | _ -> annotated    
    with 
    | exn -> ownerTy

let typedFileSystem =
    erasedType<obj> thisAssembly rootNamespace "FileSystem"
      |> staticParameter "path" (fun typeName path -> 
            annotateAsFileSystemInfo 
              (new DirectoryInfo(path))
              (erasedType<obj> thisAssembly rootNamespace typeName)
              ())