module FSharpx.TypeProviders.Settings

open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Reflection

// Get the assembly and namespace used to house the provided types
let thisAssembly = Assembly.GetExecutingAssembly()
let rootNamespace = "FSharpx"