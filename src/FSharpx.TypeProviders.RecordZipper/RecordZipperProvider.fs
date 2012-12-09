module FSharpx.TypeProviders.RecordZipperProvider

open FSharpx.TypeProviders.Helper
open System.Reflection
open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

let internal recordZipperType ownerType (cfg:TypeProviderConfig) =
    let recordZipperType = erasedType<obj> thisAssembly rootNamespace "RecordZipper"
    recordZipperType.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("filename", typeof<string>)], 
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as fileName |] -> 
                
                let resolvedFileName = findConfigFile cfg.ResolutionFolder fileName
                watchForChanges ownerType resolvedFileName
                                
                let recordZipperType = erasedType<obj> thisAssembly rootNamespace typeName 

                match CompilerWrapper.getRecords resolvedFileName with
                | Some records ->
                    for record in records do
                        let newRecord = ProvidedTypeDefinition(record.Name, Some record.BaseType)
                        recordZipperType.AddMember newRecord

                | None -> ()

                recordZipperType))

    recordZipperType

[<TypeProvider>]
type public DocumentProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        rootNamespace, 
        [recordZipperType this cfg])

[<TypeProviderAssembly>]
do ()
