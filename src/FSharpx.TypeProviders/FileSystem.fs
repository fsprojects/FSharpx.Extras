module FSharpx.TypeProviders.FilesTypeProvider

// originally from https://gist.github.com/1241061

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings

let baseTy = typeof<obj>
let rec addMembers (path:string) (ownerTy:ProvidedTypeDefinition) = 
    ownerTy.AddXmlDoc "A strongly typed interface to the directory '%s'"
    let dir = new System.IO.DirectoryInfo(path)
    let pathField = ProvidedLiteralField("Path", typeof<string>, dir.FullName);
    pathField.AddXmlDoc "Full path to fullName"
    ownerTy.AddMember pathField
    for sub in dir.EnumerateDirectories() do
                let subTy = ProvidedTypeDefinition(
                                typeName = sub.Name.Replace(' ','_'), 
                                baseType = Some baseTy, 
                                HideObjectMethods = true)
                let pathField = ProvidedLiteralField("Path", typeof<string>, sub.FullName)
                pathField.AddXmlDoc "Full path to fullName"
                subTy.AddMember pathField
                addMembersSafe sub.FullName subTy
                ownerTy.AddMember subTy
    for file in dir.EnumerateFiles() do
                let subTy = ProvidedTypeDefinition(
                                typeName = file.Name.Replace(' ','_'), 
                                baseType = Some baseTy, 
                                HideObjectMethods = true)
                let pathField = ProvidedLiteralField("Path", typeof<string>, file.FullName)
                pathField.AddXmlDoc "Full path to fullName"
                subTy.AddMember pathField
                ownerTy.AddMember subTy
and addMembersSafe (path:string) (ownerTy:ProvidedTypeDefinition) = 
    try
        addMembers path ownerTy
    with | exn ->
            ()


let fileTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "FileTyped", Some baseTy)

do fileTy.DefineStaticParameters(
    parameters=[ProvidedStaticParameter("path", typeof<string>)], 
    instantiationFunction=(fun typeName parameterValues ->

        match parameterValues with 
        | [| :? string as path |] -> 
        let ty = ProvidedTypeDefinition(
                    assembly = thisAssembly, 
                    namespaceName = rootNamespace, 
                    typeName = typeName, 
                    baseType = Some baseTy, 
                    HideObjectMethods = true)
            
            
        addMembersSafe path ty
                    
        ty
        | _ -> failwith "unexpected parameter values")) 