module FSharpx.TypeProviders.Settings

open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Reflection

// Get the assembly and namespace used to house the provided types
let thisAssembly = Assembly.GetExecutingAssembly()
let rootNamespace = "FSharpx"
let objectBaseType = typeof<obj>

// Starting to implement a DSL on top of ProvidedTypes API

let hideOldMethods (providedDefinition:ProvidedTypeDefinition) = 
    providedDefinition.HideObjectMethods <- true
    providedDefinition

let runtimeType<'a> name = ProvidedTypeDefinition(typeName = name, baseType = Some typeof<'a>)

let erasedType<'a> name = ProvidedTypeDefinition(thisAssembly, rootNamespace, name, Some typeof<'a>)