module internal FSharpx.TypeProviders.JsonTypeProvider

open System
open System.IO
open FSharpx.TypeProviders.Helper
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Inference
open FSharpx.JSON

let dict = new System.Collections.Generic.Dictionary<_,_>()

// Generates type for an inferred JSON document
let rec generateType (ownerType:ProvidedTypeDefinition) (CompoundProperty(elementName,multiProperty,elementChildren,elementProperties)) =
    let append =
        match dict.TryGetValue elementName with
        | true,c -> dict.[elementName] <- c + 1; c.ToString()
        | _ -> dict.Add(elementName,1); ""

    let ty = runtimeType<JsonValue> (elementName + append)
    ownerType.AddMember(ty)

    let accessExpr propertyName propertyType (args: Expr list) = 
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ (%%args.[0]: JsonValue).GetText propertyName @@>
        | x when x = typeof<bool> -> 
            <@@ (%%args.[0]: JsonValue).GetBoolean propertyName @@>
        | x when x = typeof<int> -> 
            <@@ (%%args.[0]: JsonValue).GetDecimal propertyName |> int @@>
        | x when x = typeof<int64> -> 
            <@@ (%%args.[0]: JsonValue).GetDecimal propertyName |> int64 @@>
        | x when x = typeof<decimal> -> 
            <@@ (%%args.[0]: JsonValue).GetDecimal propertyName @@>
        | x when x = typeof<float> -> 
            <@@ (%%args.[0]: JsonValue).GetDouble propertyName @@>
        | x when x = typeof<DateTime> -> 
            <@@ (%%args.[0]: JsonValue).GetDate propertyName @@>

    let checkIfOptional propertyName (args: Expr list) = 
        <@@ (%%args.[0]: JsonValue).HasProperty propertyName @@>

    let setterExpr propertyName propertyType (args: Expr list) = 
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ (%%args.[0]: JsonValue).AddStringProperty(propertyName,(%%args.[1]:string))  @@>
        | x when x = typeof<bool> -> 
            <@@ (%%args.[0]: JsonValue).AddBoolProperty(propertyName,(%%args.[1]:bool))  @@>
        | x when x = typeof<int> ->
            <@@ (%%args.[0]: JsonValue).AddDecimalProperty(propertyName,decimal (%%args.[1]:int))  @@>
        | x when x = typeof<decimal> ->
            <@@ (%%args.[0]: JsonValue).AddDecimalProperty(propertyName,(%%args.[1]:decimal))  @@>
        | x when x = typeof<int64> ->
            <@@ (%%args.[0]: JsonValue).AddDecimalProperty(propertyName,decimal (%%args.[1]:int64)) @@>
        | x when x = typeof<float> ->
            <@@ (%%args.[0]: JsonValue).AddDoubleProperty(propertyName,(%%args.[1]:float)) @@>
        | x when x = typeof<DateTime> -> 
            <@@ (%%args.[0]: JsonValue).AddDateProperty(propertyName,(%%args.[1]:DateTime))  @@>

    let optionalSetterExpr propertyName propertyType (args: Expr list) =         
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ match (%%args.[1]:string option) with
                | Some text -> (%%args.[0]: JsonValue).AddStringProperty(propertyName,text)
                | None -> (%%args.[0]: JsonValue).RemoveProperty propertyName @@>
        | x when x = typeof<bool> -> 
            <@@ match (%%args.[1]:bool option) with
                | Some boolean -> (%%args.[0]: JsonValue).AddBoolProperty(propertyName,boolean) 
                | None -> (%%args.[0]: JsonValue).RemoveProperty propertyName @@>
        | x when x = typeof<int> -> 
            <@@ match (%%args.[1]:int option) with
                | Some number -> (%%args.[0]: JsonValue).AddDecimalProperty(propertyName,decimal number)
                | None -> (%%args.[0]: JsonValue).RemoveProperty propertyName @@>
        | x when x = typeof<int64> -> 
            <@@ match (%%args.[1]:int64 option) with
                | Some number -> (%%args.[0]: JsonValue).AddDecimalProperty(propertyName,decimal number)
                | None -> (%%args.[0]: JsonValue).RemoveProperty propertyName @@>
        | x when x = typeof<float> -> 
            <@@ match (%%args.[1]:float option) with
                | Some number -> (%%args.[0]: JsonValue).AddDoubleProperty(propertyName,number) 
                | None -> (%%args.[0]: JsonValue).RemoveProperty propertyName @@>
        | x when x = typeof<decimal> -> 
            <@@ match (%%args.[1]:decimal option) with
                | Some number -> (%%args.[0]: JsonValue).AddDecimalProperty(propertyName,number) 
                | None -> (%%args.[0]: JsonValue).RemoveProperty propertyName @@>
        | x when x = typeof<DateTime> -> 
            <@@ match (%%args.[1]:DateTime option) with
                | Some date -> (%%args.[0]: JsonValue).AddDateProperty(propertyName,date) 
                | None -> (%%args.[0]: JsonValue).RemoveProperty propertyName @@>

    generateProperties ty accessExpr checkIfOptional setterExpr optionalSetterExpr elementProperties
    
    let multiAccessExpr childName (args: Expr list) = <@@ (%%args.[0]: JsonValue).GetArrayElements childName @@>
    let singleAccessExpr childName (args: Expr list) = <@@ (%%args.[0]: JsonValue).GetProperty childName @@>
    let newChildExpr childName (args: Expr list) = <@@ JsonValue.Obj(Map.empty) @@>

    let addChildExpr childName (args: Expr list) = <@@ (%%args.[0]: JsonValue).AddArrayElement(childName,(%%args.[1]:JsonValue)) @@>

    generateSublements ty ownerType multiAccessExpr addChildExpr newChildExpr singleAccessExpr generateType elementChildren

open System.Xml
open System.Xml.Linq

/// Infer schema from the loaded data and generate type with properties
let jsonType (ownerType:TypeProviderForNamespaces) (cfg:TypeProviderConfig) =
    let missingValue = "@@@missingValue###"
    let jsonDocumentType = erasedType<obj> thisAssembly rootNamespace "StructuredJSON"
    jsonDocumentType.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("FileName", typeof<string>, missingValue)   // Parameterize the type by the file to use as a template
                      ProvidedStaticParameter("Schema" , typeof<string>, missingValue) ], // Allows to specify inlined schema
        instantiationFunction = 
            (fun typeName parameterValues ->
                let schema = 
                    match parameterValues with 
                    | [| :? string as fileName; :? string |] when fileName <> missingValue ->        
                        let resolvedFileName = findConfigFile cfg.ResolutionFolder fileName
                        watchForChanges ownerType resolvedFileName
                                       
                        resolvedFileName |> File.ReadAllText
                    | [| :? string; :? string as schema |] when schema <> missingValue -> schema
                    | _ -> failwith "You have to specify a filename or inlined Schema"
                    
                let jsonDocumentType =
                    { Schema = JSONInference.provideElement "Document" false [parse schema]
                      EmptyConstructor = fun args -> <@@ parse schema @@>
                      FileNameConstructor = fun args -> <@@ (%%args.[0] : string) |> File.ReadAllText |> parse  @@>
                      DocumentContentConstructor = fun args -> <@@ (%%args.[0] : string) |> parse  @@>
                      RootPropertyGetter = fun args -> <@@ (%%args.[0] : JsonValue) @@>
                      ToStringExpr = fun args -> <@@ (%%args.[0]: JsonValue).ToString() @@> }
                    |> createParserType<JsonValue> typeName generateType     
        
                let converterMethod =
                    ProvidedMethod(
                        methodName = "ToXml",
                        parameters = [],
                        returnType = typeof<XObject seq>,
                        InvokeCode = (fun args -> <@@ (%%args.[0]: JsonValue).ToXml() @@>))

                converterMethod.AddXmlDoc "Gets the XML representation"

                jsonDocumentType.AddMember converterMethod
                jsonDocumentType))
    jsonDocumentType