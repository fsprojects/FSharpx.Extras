// ----------------------------------------------------------------------------
// Original Xml type provider
// (c) Tomas Petricek - tomasP.net, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
module internal FSharpx.TypeProviders.XmlTypeProvider

open System
open System.IO
open FSharpx.TypeProviders.Helper
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Inference
open System.Xml.Linq

let dict = new System.Collections.Generic.Dictionary<_,_>()

/// Generates type for an inferred XML element
let rec generateType (ownerType:ProvidedTypeDefinition) (CompoundProperty(elementName,multi,elementChildren,elementProperties)) =
    let append =
        match dict.TryGetValue elementName with
        | true,c -> dict.[elementName] <- c + 1; c.ToString()
        | _ -> dict.Add(elementName,1); ""

    let ty = runtimeType<TypedXElement> (elementName + append)
    ownerType.AddMember(ty)

    let accessExpr propertyName propertyType (args: Expr list) = 
        match propertyType with
        | x when x = typeof<bool> ->
            <@@  let (s:string) = (%%args.[0]:TypedXElement).Element.Attribute(XName.op_Implicit propertyName).Value
                 s.Equals("true", StringComparison.InvariantCultureIgnoreCase) ||
                 s.Equals("yes", StringComparison.InvariantCultureIgnoreCase)  @@> 
        | x when x = typeof<int> ->
            <@@  (%%args.[0]:TypedXElement).Element.Attribute(XName.op_Implicit propertyName).Value
                    |> Int32.Parse @@> 
        | x when x = typeof<int64> ->
            <@@  (%%args.[0]:TypedXElement).Element.Attribute(XName.op_Implicit propertyName).Value
                    |> Int64.Parse @@> 
        | x when x = typeof<float> ->
            <@@  (%%args.[0]:TypedXElement).Element.Attribute(XName.op_Implicit propertyName).Value
                    |> Double.Parse @@> 
        | x when x = typeof<string> ->
            <@@  (%%args.[0]:TypedXElement).Element.Attribute(XName.op_Implicit propertyName).Value @@> 

    let checkIfOptional propertyName (args: Expr list) = 
        <@@ (%%args.[0]:TypedXElement).Element.Attribute(XName.op_Implicit propertyName) <> null @@>

    let setterExpr propertyName propertyType (args: Expr list) = 
        match propertyType with
        | x when x = typeof<bool> ->
            <@@  (%%args.[0]:TypedXElement).Element.SetAttributeValue(XName.op_Implicit propertyName, (%%args.[1]:bool).ToString()) @@>
        | x when x = typeof<int> ->
            <@@  (%%args.[0]:TypedXElement).Element.SetAttributeValue(XName.op_Implicit propertyName, (%%args.[1]:int).ToString()) @@>
        | x when x = typeof<int64> ->
            <@@  (%%args.[0]:TypedXElement).Element.SetAttributeValue(XName.op_Implicit propertyName, (%%args.[1]:int64).ToString()) @@>
        | x when x = typeof<float> ->
            <@@  (%%args.[0]:TypedXElement).Element.SetAttributeValue(XName.op_Implicit propertyName, (%%args.[1]:float).ToString()) @@>
        | x when x = typeof<string> ->
            <@@  (%%args.[0]:TypedXElement).Element.SetAttributeValue(XName.op_Implicit propertyName, (%%args.[1]:string)) @@>

    let optionalSetterExpr propertyName propertyType (args: Expr list) = raise <| new NotImplementedException()

    generateProperties ty accessExpr checkIfOptional setterExpr optionalSetterExpr elementProperties

    let multiAccessExpr childName (args: Expr list) =
        <@@ seq { for e in ((%%args.[0]:TypedXElement).Element.Elements(XName.op_Implicit childName)) -> 
                                TypedXElement(e) } @@>

    let newChildExpr childName (args: Expr list) =
        <@@ TypedXElement(new XElement(XName.op_Implicit childName)) @@>

    let addChildExpr childName (args: Expr list) =
        <@@ (%%args.[0]:TypedXElement).Element.Add((%%args.[1]:TypedXElement).Element) @@>

    let singleAccessExpr childName (args: Expr list) = raise <| new NotImplementedException()

    generateSublements ty ownerType multiAccessExpr addChildExpr newChildExpr singleAccessExpr generateType elementChildren   

open FSharpx.JSON
open FSharpx.JSON.DocumentExtensions

/// Infer schema from the loaded data and generate type with properties
let xmlType (ownerType:TypeProviderForNamespaces) (cfg:TypeProviderConfig) =  
    let xmlDocumentType = erasedType<obj> thisAssembly rootNamespace "StructuredXml"
    xmlDocumentType.DefineStaticParameters(
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
                    
                let doc = XDocument.Parse schema
                let xmlDocumentType =
                    { Schema = XmlInference.provideElement doc.Root.Name.LocalName [doc.Root]
                      EmptyConstructor = fun args -> <@@ TypedXDocument(XDocument.Parse schema) @@>
                      FileNameConstructor = fun args -> <@@ TypedXDocument(XDocument.Load(%%args.[0] : string)) @@>
                      DocumentContentConstructor = fun args -> <@@ TypedXDocument(XDocument.Parse(%%args.[0] : string)) @@>
                      RootPropertyGetter = fun args -> <@@ TypedXElement((%%args.[0] : TypedXDocument).Document.Root) @@>
                      ToStringExpr = fun args -> <@@ (%%args.[0]: TypedXDocument).Document.ToString() @@> }
                    |> createParserType<TypedXDocument> typeName generateType     
        
                let converterMethod =
                    ProvidedMethod(
                        methodName = "ToJson",
                        parameters = [],
                        returnType = typeof<IDocument>,
                        InvokeCode = (fun args -> <@@ (%%args.[0]: TypedXDocument).Document.ToJson() @@>))

                converterMethod.AddXmlDoc "Gets the JSON representation"

                xmlDocumentType.AddMember converterMethod
                xmlDocumentType))
    xmlDocumentType