module FSharpx.TypeProviders.DSL

open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Reflection

// Starting to implement a DSL on top of ProvidedTypes API

let hideOldMethods (providedDefinition:ProvidedTypeDefinition) = 
    providedDefinition.HideObjectMethods <- true
    providedDefinition

let runtimeType<'a> typeName = ProvidedTypeDefinition(typeName = typeName, baseType = Some typeof<'a>)

let erasedType<'a> assemblyName rootNamespace typeName = 
    ProvidedTypeDefinition(assemblyName, rootNamespace, typeName, Some typeof<'a>)