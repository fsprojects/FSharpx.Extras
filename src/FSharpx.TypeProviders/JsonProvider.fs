module FSharpx.TypeProviders.JsonTypeProvider

open System
open System.IO
open FSharpx.TypeProviders.DSL
open FSharpx.TypeProviders.Settings
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Inference
open FSharpx.JSON

// Generates type for an inferred JSON document
let rec generateType (ownerType:ProvidedTypeDefinition) (CompoundProperty(elementName,multiProperty,elementChildren,elementProperties)) =
    let ty = runtimeType<Document> elementName
    ownerType.AddMember(ty)

    let accessExpr propertyName propertyType (args: Expr list) = 
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ (%%args.[0]: Document).GetText propertyName @@>
        | x when x = typeof<bool> -> 
            <@@ (%%args.[0]: Document).GetBoolean propertyName @@>
        | x when x = typeof<int> -> 
            <@@ (%%args.[0]: Document).GetNumber propertyName |> int @@>
        | x when x = typeof<float> -> 
            <@@ (%%args.[0]: Document).GetNumber propertyName @@>

    let checkIfOptional propertyName (args: Expr list) = 
        <@@ (%%args.[0]: Document).HasProperty propertyName @@>

    let setterExpr propertyName propertyType (args: Expr list) = 
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ (%%args.[0]: Document).AddTextProperty(propertyName,(%%args.[1]:string)) |> ignore @@>
        | x when x = typeof<bool> -> 
            <@@ (%%args.[0]: Document).AddBoolProperty(propertyName,(%%args.[1]:bool)) |> ignore @@>
        | x when x = typeof<int> ->
            <@@ (%%args.[0]: Document).AddNumberProperty(propertyName,float (%%args.[1]:int)) |> ignore @@>
        | x when x = typeof<float> ->
            <@@ (%%args.[0]: Document).AddNumberProperty(propertyName,(%%args.[1]:float)) |> ignore @@>

    let optionalSetterExpr propertyName propertyType (args: Expr list) =         
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ match (%%args.[1]:string option) with
                | Some text -> (%%args.[0]: Document).AddTextProperty(propertyName,text) |> ignore
                | None -> (%%args.[0]: Document).RemoveProperty propertyName @@>
        | x when x = typeof<bool> -> 
            <@@ match (%%args.[1]:bool option) with
                | Some boolean -> (%%args.[0]: Document).AddBoolProperty(propertyName,boolean) |> ignore
                | None -> (%%args.[0]: Document).RemoveProperty propertyName @@>
        | x when x = typeof<int> -> 
            <@@ match (%%args.[1]:int option) with
                | Some number -> (%%args.[0]: Document).AddNumberProperty(propertyName,float number) |> ignore
                | None -> (%%args.[0]: Document).RemoveProperty propertyName @@>
        | x when x = typeof<float> -> 
            <@@ match (%%args.[1]:float option) with
                | Some number -> (%%args.[0]: Document).AddNumberProperty(propertyName,number) |> ignore
                | None -> (%%args.[0]: Document).RemoveProperty propertyName @@>

    generateProperties ty accessExpr checkIfOptional setterExpr optionalSetterExpr elementProperties
    
    let multiAccessExpr childName (args: Expr list) = <@@ (%%args.[0]: Document).GetJArray(childName).Elements @@>
    let singleAccessExpr childName (args: Expr list) = <@@ (%%args.[0]: Document).GetProperty childName @@>
    let newChildExpr childName (args: Expr list) = <@@ JObject.New() @@>

    let addChildExpr childName (args: Expr list) = <@@ (%%args.[0]: Document).GetJArray(childName).Elements.Add(%%args.[1]:Document) @@>

    generateSublements ty ownerType multiAccessExpr addChildExpr newChildExpr singleAccessExpr generateType elementChildren

/// Infer schema from the loaded data and generate type with properties
let jsonType (ownerType:TypeProviderForNamespaces) cfg =      
    let createTypeFromSchema typeName (jsonText:string) =        
        createParserType<Document> 
            typeName 
            (JSONInference.provideElement "Document" false [parse jsonText])
            generateType
            (fun args -> <@@ jsonText |> parse  @@>)
            (fun args -> <@@ (%%args.[0] : string) |> File.ReadAllText |> parse  @@>)
            (fun args -> <@@ (%%args.[0] : Document) @@>)

    let createTypeFromFileName typeName (fileName:string) =
        System.IO.File.ReadAllText fileName
        |> createTypeFromSchema typeName

    createStructuredParser thisAssembly rootNamespace "StructuredJSON" cfg ownerType createTypeFromFileName createTypeFromSchema