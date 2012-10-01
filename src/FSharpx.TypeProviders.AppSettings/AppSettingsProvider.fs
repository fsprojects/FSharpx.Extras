module FSharpx.TypeProviders.AppSettingsTypeProvider

open FSharpx
open FSharpx.TypeProviders.DSL
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

let internal addTypedAppSettings (cfg:TypeProviderConfig) (configFileName:string) (tyDef:ProvidedTypeDefinition) = 
    try        
        let filePath = findConfigFile cfg.ResolutionFolder configFileName
        let fileMap = ExeConfigurationFileMap(ExeConfigFilename=filePath)
        let appSettings = ConfigurationManager.OpenMappedExeConfiguration(fileMap, ConfigurationUserLevel.None).AppSettings.Settings

        tyDef
          |++!>
            (appSettings.AllKeys 
              |> Seq.map (fun key -> 
                        let field =
                            match (appSettings.Item key).Value with
                            | Int fieldValue -> literalField key fieldValue
                            | Bool fieldValue -> literalField key fieldValue
                            | Double fieldValue -> literalField key fieldValue
                            | fieldValue -> literalField key fieldValue
                        field
                        |> addLiteralXmlDoc (sprintf "Returns the value from %s with key %s" configFileName key)
                        |> addLiteralDefinitionLocation (fileStart configFileName)))
    with 
    | exn -> tyDef
    
let internal typedAppSettings (cfg:TypeProviderConfig) =
    erasedType<obj> thisAssembly rootNamespace "AppSettings"
      |> staticParameter "configFileName" (fun typeName configFileName -> 
            erasedType<obj> thisAssembly rootNamespace typeName
                |> addTypedAppSettings cfg configFileName )


[<TypeProvider>]
type public FSharpxProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(DSL.rootNamespace,[typedAppSettings cfg])

[<TypeProviderAssembly>]
do ()