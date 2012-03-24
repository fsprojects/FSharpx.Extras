module FSharpx.TypeProviders.JsonTypeProvider

open System
open System.IO
open FSharpx.TypeProviders.DSL
open FSharpx.TypeProviders.Settings
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

    let setterExpr propertyName propertyType (args: Expr list) = 
        raise <| new NotImplementedException()

    generateProperties ty accessExpr checkIfOptional setterExpr elementProperties
    
    let multiAccessExpr childName (args: Expr list) = <@@ (%%args.[0]:JSON).GetProperty(childName).GetSubElements() @@>
    let singleAccessExpr childName (args: Expr list) = <@@ ((%%args.[0]:JSON).GetProperty childName) @@>
    let newChildExpr childName (args: Expr list) = raise <| new NotImplementedException()
    let addChildExpr (args: Expr list) = raise <| new NotImplementedException()

    generateSublements ty ownerType multiAccessExpr addChildExpr newChildExpr singleAccessExpr generateType elementChildren

/// Infer schema from the loaded data and generate type with properties
let jsonType (ownerType:TypeProviderForNamespaces) cfg =      
    let createTypeFromSchema typeName (jsonText:string) =        
        createParserType<JSON> 
            typeName 
            (JSONInference.provideElement "Document" false [parse jsonText])
            generateType
            (fun args -> <@@ jsonText |> parse  @@>)
            (fun args -> <@@ (%%args.[0] : string) |> File.ReadAllText |> parse  @@>)
            (fun args -> <@@ (%%args.[0] : JSON) @@>)

    let createTypeFromFileName typeName (fileName:string) =
        System.IO.File.ReadAllText fileName
        |> createTypeFromSchema typeName

    createStructuredParser thisAssembly rootNamespace "StructuredJSON" cfg ownerType createTypeFromFileName createTypeFromSchema