/// originally from https://gist.github.com/1241061
module internal FSharpx.TypeProviders.FilesTypeProvider

open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Helper
open System.IO


let createTypedFileSystem() = 
    let typedFileSystem = erasedType<obj> thisAssembly rootNamespace "FileSystem"

    typedFileSystem.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("path", typeof<string>)], 
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as path |] ->
                let startDir = new DirectoryInfo(path)
                let fileSystemType = erasedType<obj> thisAssembly rootNamespace typeName

                let rec annotateAsFileSystemInfo (fileSystemInfo:FileSystemInfo,ownerType:ProvidedTypeDefinition) () =
                    try        
                        ownerType.AddXmlDoc(sprintf "A strongly typed interface to '%s'" fileSystemInfo.FullName)
                        ownerType.HideObjectMethods <- true
                        let field = ProvidedLiteralField("Path",typeof<string>,fileSystemInfo.FullName)
                        field.AddXmlDoc(sprintf "Full path to '%s'" fileSystemInfo.FullName)
                        ownerType.AddMember(field)
        
                        match fileSystemInfo with
                        | :? DirectoryInfo as dir ->
                                for info in dir.EnumerateFileSystemInfos() do
                                let directoryType = runtimeType<obj> info.Name
                                ownerType.AddMemberDelayed(annotateAsFileSystemInfo(info,directoryType))
                        | _ -> ()

                        ownerType
                    with 
                    | exn -> ownerType

                annotateAsFileSystemInfo(startDir,fileSystemType)()))

    typedFileSystem