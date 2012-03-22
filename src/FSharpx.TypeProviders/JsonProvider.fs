module FSharpx.TypeProviders.JsonTypeProvider

open System
open System.IO
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Inference
open FSharpx.TypeProviders.JSONParser


// Generates type for an inferred JSON document
let rec generateType (ownerType:ProvidedTypeDefinition) (CompoundProperty(elementName,multiProperty,elementChildren,elementProperties)) =
    let ty = runtimeType<JSON> elementName
    ownerType.AddMember(ty)

    let accessExpr propertyName propertyType (args: Expr list) = 
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ (%%args.[0]:JSON).GetProperty(propertyName).GetText() @@>
        | x when x = typeof<bool> -> 
            <@@ (%%args.[0]:JSON).GetProperty(propertyName).GetBoolean() @@>
        | x when x = typeof<int> -> 
            <@@ (%%args.[0]:JSON).GetProperty(propertyName).GetNumber() |> int @@>
        | x when x = typeof<float> -> 
            <@@ (%%args.[0]:JSON).GetProperty(propertyName).GetNumber() @@>


    let checkIfOptional propertyName (args: Expr list) = 
        <@@ (%%args.[0]:JSON).HasProperty propertyName @@>

    generateProperties ty accessExpr checkIfOptional elementProperties
    
    let multiAccessExpr childName (args: Expr list) = <@@ (%%args.[0]:JSON).GetProperty(childName).GetSubElements() @@>
    let singleAccessExpr childName (args: Expr list) = <@@ ((%%args.[0]:JSON).GetProperty childName) @@>

    generateSublements ty ownerType multiAccessExpr singleAccessExpr generateType elementChildren


let jsonType (ownerType:TypeProviderForNamespaces) (cfg:TypeProviderConfig) =  
    /// Infer schema from the loaded data and generate type with properties
    let createType typeName (jsonText:string) =        
        createParserType<JSON> 
            typeName 
            (JSONInference.provideElement "Document" false [parse jsonText])
            generateType
            (fun args -> <@@ jsonText |> parse  @@>)
            (fun args -> <@@ (%%args.[0] : string) |> File.ReadAllText |> parse  @@>)
            (fun args -> <@@ (%%args.[0] : JSON) @@>)

    createStructuredParser "StructuredJSON" cfg.ResolutionFolder ownerType createType