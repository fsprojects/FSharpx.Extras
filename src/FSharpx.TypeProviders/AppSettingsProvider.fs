module FSharpx.TypeProviders.AppSettingsTypeProvider

open FSharpx
open FSharpx.TypeProviders.DSL
open FSharpx.TypeProviders.Settings
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System
open System.Configuration
open System.IO
open System.Reflection

let (|Bool|_|) = Option.tryParseWith Boolean.TryParse
let (|Int|_|) = Option.tryParseWith Int32.TryParse
let (|Double|_|) = Option.tryParseWith Double.TryParse

let addTypedAppSettings (cfg:TypeProviderConfig) (configFileName:string) (tyDef:ProvidedTypeDefinition) = 
    try        
        let filePath = Path.Combine(cfg.ResolutionFolder, configFileName) 
        let fileMap = ExeConfigurationFileMap(ExeConfigFilename=filePath)
        let appSettings = ConfigurationManager.OpenMappedExeConfiguration(fileMap, ConfigurationUserLevel.None).AppSettings.Settings

        Seq.iter (fun (key) ->
            tyDef 
            |+> match (appSettings.Item key).Value with
                | Int fieldValue -> literalField key fieldValue
                | Bool fieldValue -> literalField key fieldValue
                | Double fieldValue -> literalField key fieldValue
                | fieldValue -> literalField key fieldValue
            |> addXmlDoc (sprintf "Returns the value from the appSetting with key %s" key)
            |> ignore) appSettings.AllKeys 

        tyDef
    with 
    | exn -> tyDef
    
let typedAppSettings (cfg:TypeProviderConfig) =
    erasedType<obj> thisAssembly rootNamespace "AppSettingsTyped"
      |> staticParameter "configFileName" (fun typeName configFileName -> 
            erasedType<obj> thisAssembly rootNamespace typeName
                |> addTypedAppSettings cfg configFileName )
