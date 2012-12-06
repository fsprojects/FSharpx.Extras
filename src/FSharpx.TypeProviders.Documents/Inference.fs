// ----------------------------------------------------------------------------
// Original Xml type provider
// (c) Tomas Petricek - tomasP.net, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
module internal FSharpx.TypeProviders.Inference

open System
open System.Xml
open System.Xml.Linq
open FSharpx.TypeProviders.Helper
open System.Collections.Generic
open System.Globalization
open FSharpx.Strings

// ------------------------------------------------------------------------------------------------
// Representation about inferred structure
// ------------------------------------------------------------------------------------------------

type SimpleProperty = SimpleProperty of string * Type * bool

type CompoundProperty = CompoundProperty of string * bool * CompoundProperty seq * SimpleProperty seq

open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices

/// Iterates over all the sub elements, generates types for them
/// and adds member for accessing them to the parent.
let generateSublements (ownerType:ProvidedTypeDefinition) parentType multiAccessExpr addChildExpr newChildExpr singleAccessExpr generateTypeF children =
    for CompoundProperty(childName,multi,_,_) as child in children do
        let childType = generateTypeF parentType child

        if multi then     
            let newType = seqType childType
            let niceChildName = childName |> niceName |> singularize 

            let getChildrenMethod =
                ProvidedMethod(
                    methodName = "Get" + pluralize niceChildName,
                    parameters = [],
                    returnType = newType,
                    InvokeCode = multiAccessExpr childName)

            getChildrenMethod.AddXmlDoc (sprintf @"Gets the %s elements" childName)

            ownerType.AddMember getChildrenMethod

            let newChildMethod =
                ProvidedMethod(
                    methodName = "New" + niceChildName,
                    parameters = [],
                    returnType = childType,
                    InvokeCode = newChildExpr childName)

            newChildMethod.AddXmlDoc (sprintf @"Creates a new %s element" childName)

            ownerType.AddMember newChildMethod
            
            let addChildMethod =
                ProvidedMethod(
                    methodName = "Add" + niceChildName,
                    parameters = [ProvidedParameter("element", childType)],
                    returnType = typeof<unit>,
                    InvokeCode = addChildExpr childName)

            addChildMethod.AddXmlDoc (sprintf @"Adds a %s element" childName)

            ownerType.AddMember addChildMethod
        else
            let childGetter =
                ProvidedProperty(
                    propertyName = niceName childName,
                    propertyType = childType,
                    GetterCode = singleAccessExpr childName)

            childGetter.AddXmlDoc (sprintf @"Gets the %s attribute" childName)
            ownerType.AddMember childGetter


    ownerType

type ExprDef = Expr list -> Expr

type GeneratedParserSettings = {
    Schema: CompoundProperty
    EmptyConstructor: ExprDef
    FileNameConstructor: ExprDef
    DocumentContentConstructor : ExprDef
    RootPropertyGetter: ExprDef
    ToStringExpr: ExprDef }

/// Generates constructors for loading data and adds type representing Root node
let createParserType<'a> typeName (generateTypeF: ProvidedTypeDefinition -> CompoundProperty -> ProvidedTypeDefinition) settings =
    let parserType = erasedType<'a> thisAssembly rootNamespace typeName
     
    let defaultConstructor = 
        ProvidedConstructor(
            parameters = [],
            InvokeCode = settings.EmptyConstructor)
    defaultConstructor.AddXmlDoc "Initializes the document from the schema sample."

    parserType.AddMember defaultConstructor

    let fileNameConstructor = 
        ProvidedConstructor(
            parameters = [ProvidedParameter("filename", typeof<string>)],
            InvokeCode = settings.FileNameConstructor)
    fileNameConstructor.AddXmlDoc "Initializes a document from the given path."

    parserType.AddMember fileNameConstructor

    let inlinedDocumentConstructor = 
        ProvidedConstructor(
            parameters = [ProvidedParameter("documentContent", typeof<string>)],
            InvokeCode = settings.DocumentContentConstructor)
    inlinedDocumentConstructor.AddXmlDoc "Initializes a document from the given string."

    parserType.AddMember inlinedDocumentConstructor

    let rootProperty =
        ProvidedProperty(
            propertyName = "Root",
            propertyType = generateTypeF parserType settings.Schema,
            GetterCode = settings.RootPropertyGetter)

    rootProperty.AddXmlDoc "Gets the document root"

    parserType.AddMember rootProperty

    let toStringMethod =
        ProvidedMethod(
            methodName = "ToString",
            parameters = [],
            returnType = typeof<string>,
            InvokeCode = settings.ToStringExpr)

    toStringMethod.AddXmlDoc "Gets the string representation"

    parserType.AddMember toStringMethod

    parserType