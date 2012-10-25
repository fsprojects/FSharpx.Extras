/// Starting to implement some helpers on top of ProvidedTypes API
module internal FSharpx.TypeProviders.Helper

open System.IO
open FSharpx.Strings
open Samples.FSharp.ProvidedTypes

let findConfigFile resolutionFolder configFileName =
    if Path.IsPathRooted configFileName then 
        configFileName 
    else 
        Path.Combine(resolutionFolder, configFileName)

let eraseType assemblyName rootNamespace typeName toType = 
    ProvidedTypeDefinition(assemblyName, rootNamespace, typeName, Some toType)

let erasedType<'a> assemblyName rootNamespace typeName = 
    eraseType assemblyName rootNamespace typeName typeof<'a>

let runtimeType<'a> typeName = ProvidedTypeDefinition(niceName typeName, Some typeof<'a>)

// Get the assembly and namespace used to house the provided types
let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly()
let rootNamespace = "FSharpx"