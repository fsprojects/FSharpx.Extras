module FSharpx.TypeProviders.Settings

open System.Reflection

// Get the assembly and namespace used to house the provided types
let thisAssembly = Assembly.GetExecutingAssembly()
let rootNamespace = "FSharpx"
let objectBaseType = typeof<obj>    