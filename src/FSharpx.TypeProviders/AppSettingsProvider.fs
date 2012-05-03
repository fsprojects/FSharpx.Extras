module FSharpx.TypeProviders.AppSettingsTypeProvider

open FSharpx
open FSharpx.TypeProviders.DSL
open FSharpx.TypeProviders.Settings
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System
open System.Configuration
open System.IO
open System.Reflection

let (|Bool|_|) = Option.tryParseWith Boolean.TryParse
let (|Int|_|) = Option.tryParseWith Int32.TryParse
let (|Double|_|) text =  
    match Double.TryParse(text,Globalization.NumberStyles.Any,Globalization.CultureInfo.InvariantCulture) with
    | true, value -> Some value
    | _ -> None

let addTypedAppSettings (cfg:TypeProviderConfig) (configFileName:string) (tyDef:ProvidedTypeDefinition) = 
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
                        |> addXmlDoc (sprintf "Returns the value from %s with key %s" configFileName key)
                        |> addDefinitionLocation (fileStart configFileName)))
    with 
    | exn -> tyDef
    
let typedAppSettings (cfg:TypeProviderConfig) =
    erasedType<obj> thisAssembly rootNamespace "AppSettings"
      |> staticParameter "configFileName" (fun typeName configFileName -> 
            erasedType<obj> thisAssembly rootNamespace typeName
                |> addTypedAppSettings cfg configFileName )