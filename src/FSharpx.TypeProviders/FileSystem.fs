module FSharpx.TypeProviders.FilesTypeProvider

// originally from https://gist.github.com/1241061

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL

let annotateAsFileSystemType path (ownerTy:ProvidedTypeDefinition) =
    ownerTy.AddXmlDoc <| sprintf "A strongly typed interface to '%s'" path
    ownerTy
        |> hideOldMethods
        |> addMember (literalField "Path" (sprintf "Full path to '%s'" path) path)

let rec annotateWithSubdirectories path (ownerTy:ProvidedTypeDefinition) =
    try        
        let dir = new System.IO.DirectoryInfo(path)
        
        let typeWithSubdirectories =
            dir.EnumerateDirectories()
                |> Seq.map (fun sub -> 
                    runtimeType<obj> sub.Name
                      |> annotateWithSubdirectories sub.FullName)
                |> Seq.fold (fun ownerType subdirType -> addMember subdirType ownerType)
                    (annotateAsFileSystemType path ownerTy)

        dir.EnumerateFiles()
            |> Seq.map (fun file -> runtimeType<obj> file.Name |> annotateAsFileSystemType file.FullName)
            |> Seq.fold (fun ownerType fileType -> addMember fileType ownerType) typeWithSubdirectories
    
    with 
    | exn -> ownerTy

let typedFileSystem =
    erasedType<obj> thisAssembly rootNamespace "FileSystemTyped"
      |> staticParameter "path" (fun typeName path -> 
            erasedType<obj> thisAssembly rootNamespace typeName
                |> annotateWithSubdirectories path)