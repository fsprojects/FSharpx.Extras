module FSharpx.TypeProviders.FilesTypeProvider

// originally from https://gist.github.com/1241061

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings

let rec addMembers (path:string) (ownerTy:ProvidedTypeDefinition) =          
    ownerTy.AddXmlDoc "A strongly typed interface to the directory '%s'"
    let dir = new System.IO.DirectoryInfo(path)
    let pathField = ProvidedLiteralField("Path", typeof<string>, dir.FullName);
    pathField.AddXmlDoc "Full path to fullName"
    ownerTy.AddMember pathField
    for sub in dir.EnumerateDirectories() do
                let subTy = runtimeType<obj> (sub.Name.Replace(' ','_'))  |> hideOldMethods

                let pathField = ProvidedLiteralField("Path", typeof<string>, sub.FullName)
                pathField.AddXmlDoc "Full path to fullName"
                subTy.AddMember pathField
                addMembersSafe sub.FullName subTy
                ownerTy.AddMember subTy
    for file in dir.EnumerateFiles() do
                let subTy = runtimeType<obj> (file.Name.Replace(' ','_'))  |> hideOldMethods
                let pathField = ProvidedLiteralField("Path", typeof<string>, file.FullName)
                pathField.AddXmlDoc "Full path to fullName"
                subTy.AddMember pathField
                ownerTy.AddMember subTy
and addMembersSafe (path:string) (ownerTy:ProvidedTypeDefinition) = 
        try 
            addMembers path ownerTy
        with 
        | exn -> ()


let fileTy = erasedType<obj> "FileSystemTyped"

fileTy.DefineStaticParameters(
    parameters=[ProvidedStaticParameter("path", typeof<string>)], 
    instantiationFunction=(fun typeName parameterValues ->

        match parameterValues with 
        | [| :? string as path |] -> 
        let ty = erasedType<obj> typeName |> hideOldMethods
                    
        addMembersSafe path ty
                    
        ty
        | _ -> failwith "unexpected parameter values")) 