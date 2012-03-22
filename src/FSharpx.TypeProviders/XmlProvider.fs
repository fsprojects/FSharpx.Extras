// ----------------------------------------------------------------------------
// Original Xml type provider
// (c) Tomas Petricek - tomasP.net, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
module FSharpx.TypeProviders.XmlTypeProvider

open System
open System.IO
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Inference
open System.Xml.Linq

// Generates type for an inferred XML element
let rec generateType (ownerType:ProvidedTypeDefinition) (CompoundProperty(elementName,multi,elementChildren,elementProperties)) =
    let ty = runtimeType<TypedXElement> elementName
    ownerType.AddMember(ty)

    // Generate property for every inferred attribute of the element
    for SimpleProperty(propertyName,propertyType,optional) in elementProperties do 
        let accessExpr (args: Expr list) = 
            <@@  (%%args.[0]:TypedXElement).Element.
                    Attribute(XName.op_Implicit propertyName).Value @@> 
            |> convertExpr propertyType

        let prop =
            if optional then
                let newType = optionType propertyType
                // For optional elements, we return Option value
                let cases = Reflection.FSharpType.GetUnionCases newType
                let some = cases |> Seq.find (fun c -> c.Name = "Some")
                let none = cases |> Seq.find (fun c -> c.Name = "None")

                provideProperty 
                    propertyName
                    newType
                    (fun args ->
                        Expr.IfThenElse
                          (<@@ (%%args.[0]:TypedXElement).Element.
                                  Attribute(XName.op_Implicit propertyName) <> null @@>,
                            Expr.NewUnionCase(some, [accessExpr args]),
                            Expr.NewUnionCase(none, [])))
            else
                provideProperty 
                    propertyName
                    propertyType
                    accessExpr

        prop
          |> addXmlDoc (sprintf @"Gets the ""%s"" attribute" propertyName)
          |> ty.AddMember

    // Iterate over all the XML elements, generate type for them
    // and add member for accessing them to the parent.
    for CompoundProperty(childName,_,_,_) as child in elementChildren do
        let newType =
            child
            |> generateType ownerType 
            |> seqType

        ty
        |+!> (provideMethod
                ("Get" + niceName childName + "Elements")
                []
                newType
                (fun args -> 
                    <@@ seq { for e in ((%%args.[0]:TypedXElement).Element.Elements(XName.op_Implicit childName)) -> 
                                 TypedXElement(e) } @@>)
                |> addXmlDoc (sprintf @"Gets the ""%s"" elements" childName))
        |> ignore
    ty

let xmlType (ownerType:TypeProviderForNamespaces) (cfg:TypeProviderConfig) =
/// Infer schema from the loaded data and generate type with properties
    let createType typeName (xmlText:string) =        
        let doc = XDocument.Parse xmlText
        createParserType<TypedXDocument> 
            typeName 
            (XmlInference.provideElement doc.Root.Name.LocalName [doc.Root])
            generateType
            (fun args -> <@@ TypedXDocument(XDocument.Parse xmlText) @@>)
            (fun args -> <@@ TypedXDocument(XDocument.Load(%%args.[0] : string)) @@>)
            (fun args -> <@@ TypedXElement((%%args.[0] : TypedXDocument).Document.Root) @@>)
    
    createStructuredParser "StructuredXml" cfg.ResolutionFolder ownerType createType    