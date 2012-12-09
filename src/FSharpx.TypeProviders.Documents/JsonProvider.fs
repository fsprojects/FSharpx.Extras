module internal FSharpx.TypeProviders.JsonTypeProvider

open System
open System.IO
open FSharpx.TypeProviders.Helper
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Inference
open FSharpx.JSON.DocumentExtensions
open FSharpx.JSON

let dict = new System.Collections.Generic.Dictionary<_,_>()

// Generates type for an inferred JSON document
let rec generateType (ownerType:ProvidedTypeDefinition) (CompoundProperty(elementName,multiProperty,elementChildren,elementProperties)) =
    let append =
        match dict.TryGetValue elementName with
        | true,c -> dict.[elementName] <- c + 1; c.ToString()
        | _ -> dict.Add(elementName,1); ""

    let ty = runtimeType<IDocument> (elementName + append)
    ownerType.AddMember(ty)

    let accessExpr propertyName propertyType (args: Expr list) = 
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ (%%args.[0]: IDocument).GetText propertyName @@>
        | x when x = typeof<bool> -> 
            <@@ (%%args.[0]: IDocument).GetBoolean propertyName @@>
        | x when x = typeof<int> -> 
            <@@ (%%args.[0]: IDocument).GetNumber propertyName |> int @@>
        | x when x = typeof<int64> -> 
            <@@ (%%args.[0]: IDocument).GetNumber propertyName |> int64 @@>
        | x when x = typeof<float> -> 
            <@@ (%%args.[0]: IDocument).GetNumber propertyName @@>
        | x when x = typeof<DateTime> -> 
            <@@ (%%args.[0]: IDocument).GetDate propertyName @@>

    let checkIfOptional propertyName (args: Expr list) = 
        <@@ (%%args.[0]: IDocument).HasProperty propertyName @@>

    let setterExpr propertyName propertyType (args: Expr list) = 
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ (%%args.[0]: IDocument).AddTextProperty(propertyName,(%%args.[1]:string)) |> ignore @@>
        | x when x = typeof<bool> -> 
            <@@ (%%args.[0]: IDocument).AddBoolProperty(propertyName,(%%args.[1]:bool)) |> ignore @@>
        | x when x = typeof<int> ->
            <@@ (%%args.[0]: IDocument).AddNumberProperty(propertyName,float (%%args.[1]:int)) |> ignore @@>
        | x when x = typeof<int64> ->
            <@@ (%%args.[0]: IDocument).AddNumberProperty(propertyName,float (%%args.[1]:int64)) |> ignore @@>
        | x when x = typeof<float> ->
            <@@ (%%args.[0]: IDocument).AddNumberProperty(propertyName,(%%args.[1]:float)) |> ignore @@>
        | x when x = typeof<DateTime> -> 
            <@@ (%%args.[0]: IDocument).AddDateProperty(propertyName,(%%args.[1]:DateTime)) |> ignore @@>

    let optionalSetterExpr propertyName propertyType (args: Expr list) =         
        match propertyType with
        | x when x = typeof<string> -> 
            <@@ match (%%args.[1]:string option) with
                | Some text -> (%%args.[0]: IDocument).AddTextProperty(propertyName,text) |> ignore
                | None -> (%%args.[0]: IDocument).RemoveProperty propertyName @@>
        | x when x = typeof<bool> -> 
            <@@ match (%%args.[1]:bool option) with
                | Some boolean -> (%%args.[0]: IDocument).AddBoolProperty(propertyName,boolean) |> ignore
                | None -> (%%args.[0]: IDocument).RemoveProperty propertyName @@>
        | x when x = typeof<int> -> 
            <@@ match (%%args.[1]:int option) with
                | Some number -> (%%args.[0]: IDocument).AddNumberProperty(propertyName,float number) |> ignore
                | None -> (%%args.[0]: IDocument).RemoveProperty propertyName @@>
        | x when x = typeof<int64> -> 
            <@@ match (%%args.[1]:int64 option) with
                | Some number -> (%%args.[0]: IDocument).AddNumberProperty(propertyName,float number) |> ignore
                | None -> (%%args.[0]: IDocument).RemoveProperty propertyName @@>
        | x when x = typeof<float> -> 
            <@@ match (%%args.[1]:float option) with
                | Some number -> (%%args.[0]: IDocument).AddNumberProperty(propertyName,number) |> ignore
                | None -> (%%args.[0]: IDocument).RemoveProperty propertyName @@>
        | x when x = typeof<DateTime> -> 
            <@@ match (%%args.[1]:DateTime option) with
                | Some date -> (%%args.[0]: IDocument).AddDateProperty(propertyName,date) |> ignore
                | None -> (%%args.[0]: IDocument).RemoveProperty propertyName @@>

    generateProperties ty accessExpr checkIfOptional setterExpr optionalSetterExpr elementProperties
    
    let multiAccessExpr childName (args: Expr list) = <@@ (%%args.[0]: IDocument).GetJArray(childName).Elements @@>
    let singleAccessExpr childName (args: Expr list) = <@@ (%%args.[0]: IDocument).GetProperty childName @@>
    let newChildExpr childName (args: Expr list) = <@@ JObject.New() @@>

    let addChildExpr childName (args: Expr list) = <@@ (%%args.[0]: IDocument).GetJArray(childName).Elements.Add(%%args.[1]:IDocument) @@>

    generateSublements ty ownerType multiAccessExpr addChildExpr newChildExpr singleAccessExpr generateType elementChildren

open System.Xml
open System.Xml.Linq

/// Infer schema from the loaded data and generate type with properties
let jsonType (ownerType:TypeProviderForNamespaces) (cfg:TypeProviderConfig) =
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
                      RootPropertyGetter = fun args -> <@@ (%%args.[0] : IDocument) @@>
                      ToStringExpr = fun args -> <@@ (%%args.[0]: IDocument).ToString() @@> }
                    |> createParserType<IDocument> typeName generateType     
        
                let converterMethod =
                    ProvidedMethod(
                        methodName = "ToXml",
                        parameters = [],
                        returnType = typeof<XObject seq>,
                        InvokeCode = (fun args -> <@@ (%%args.[0]: IDocument).ToXml() @@>))

                converterMethod.AddXmlDoc "Gets the XML representation"

                jsonDocumentType.AddMember converterMethod
                jsonDocumentType))
    jsonDocumentType