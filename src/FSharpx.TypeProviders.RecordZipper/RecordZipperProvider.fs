module FSharpx.TypeProviders.RecordZipperProvider

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open FSharpx.TypeProviders.Helper
open Samples.FSharp.ProvidedTypes


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
                        for field in FSharpType.GetRecordFields(record) do
                            let newField = ProvidedProperty(field.Name, field.PropertyType)
                            newRecord.AddMember newField
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
