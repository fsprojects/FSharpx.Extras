// ----------------------------------------------------------------------------
// Original Xml type provider
// (c) Tomas Petricek - tomasP.net, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
module FSharpx.TypeProviders.XmlTypeProvider

open System
open FSharpx.TypeProviders.DSL
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Inference
open System.Xml.Linq

// Generates type for an inferred XML element
let rec generateType (ownerType:ProvidedTypeDefinition) (CompoundProperty(elementName,multi,elementChildren,elementProperties)) =
    let ty = runtimeType<TypedXElement> elementName
    ownerType.AddMember(ty)

    let accessExpr propertyName propertyType (args: Expr list) = 
        <@@  (%%args.[0]:TypedXElement).Element.
                Attribute(XName.op_Implicit propertyName).Value @@> 
        |> convertExpr propertyType

    let checkIfOptional propertyName (args: Expr list) = 
        <@@ (%%args.[0]:TypedXElement).Element.Attribute(XName.op_Implicit propertyName) <> null @@>

    generateProperties ty accessExpr checkIfOptional elementProperties

    let multiAccessExpr childName (args: Expr list) =
        <@@ seq { for e in ((%%args.[0]:TypedXElement).Element.Elements(XName.op_Implicit childName)) -> 
                                TypedXElement(e) } @@>

    let singleAccessExpr childName (args: Expr list) = raise <| new NotImplementedException()

    generateSublements ty ownerType multiAccessExpr singleAccessExpr generateType elementChildren   

/// Infer schema from the loaded data and generate type with properties
let xmlType (ownerType:TypeProviderForNamespaces) cfg =    
    let createType typeName (xmlText:string) =        
        let doc = XDocument.Parse xmlText
        createParserType<TypedXDocument> 
            typeName 
            (XmlInference.provideElement doc.Root.Name.LocalName [doc.Root])
            generateType
            (fun args -> <@@ TypedXDocument(XDocument.Parse xmlText) @@>)
            (fun args -> <@@ TypedXDocument(XDocument.Load(%%args.[0] : string)) @@>)
            (fun args -> <@@ TypedXElement((%%args.[0] : TypedXDocument).Document.Root) @@>)
    
    createStructuredParser "StructuredXml" cfg ownerType createType    