module FSharpx.TypeProviders.AppSettingsTypeProvider

open FSharpx
open FSharpx.TypeProviders.Helper
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System
open System.Configuration
open System.IO
open System.Reflection

/// Converts a function returning bool,value to a function returning value option.
/// Useful to process TryXX style functions.
let inline tryParseWith func = func >> function
    | true, value -> Some value
    | false, _ -> None

let (|Bool|_|) = tryParseWith Boolean.TryParse
let (|Int|_|) = tryParseWith Int32.TryParse
let (|Double|_|) text =  
    match Double.TryParse(text,Globalization.NumberStyles.Any,Globalization.CultureInfo.InvariantCulture) with
    | true, value -> Some value
    | _ -> None


let internal typedAppSettings (cfg:TypeProviderConfig) =
    let appSettings = erasedType<obj> thisAssembly rootNamespace "AppSettings"

    appSettings.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("configFileName", typeof<string>)], 
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as configFileName |] -> 
                let typeDef = erasedType<obj> thisAssembly rootNamespace typeName
                try
                    let filePath = findConfigFile cfg.ResolutionFolder configFileName
                    let fileMap = ExeConfigurationFileMap(ExeConfigFilename=filePath)
                    let appSettings = ConfigurationManager.OpenMappedExeConfiguration(fileMap, ConfigurationUserLevel.None).AppSettings.Settings

                    for key in appSettings.AllKeys do
                        let field =
                            match (appSettings.Item key).Value with
                            | Int fieldValue -> literalField key fieldValue
                            | Bool fieldValue -> literalField key fieldValue
                            | Double fieldValue -> literalField key fieldValue
                            | fieldValue -> literalField key fieldValue

                        field.AddXmlDoc (sprintf "Returns the value from %s with key %s" configFileName key)
                        field.AddDefinitionLocation(1,1,configFileName)

                        typeDef.AddMember field

                    typeDef
                with 
                | exn -> typeDef
            | x -> failwithf "unexpected parameter values %A" x))

    appSettings


[<TypeProvider>]
type public FSharpxProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(rootNamespace,[typedAppSettings cfg])

[<TypeProviderAssembly>]
do ()