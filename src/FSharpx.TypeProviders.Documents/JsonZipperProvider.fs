module internal FSharpx.TypeProviders.JsonZipperTypeProvider

open System
open System.IO
open FSharpx.TypeProviders.Helper
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Inference
open FSharpx.JSON
open FSharpx.JSON.Zipper
open FSharpx.Strings

open System.Xml
open System.Xml.Linq


let dict = new System.Collections.Generic.Dictionary<_,_>()

/// Generates type for an inferred XML element
let rec generateType (ownerType:ProvidedTypeDefinition) elementName =
    let append =
        match dict.TryGetValue elementName with
        | true,c -> dict.[elementName] <- c + 1; c.ToString()
        | _ -> dict.Add(elementName,1); ""

    let ty = runtimeType<JsonZipper> (elementName + append)
    ownerType.AddMember(ty)
    ty

/// Infer schema from the loaded data and generate type with properties
let jsonType (ownerType:TypeProviderForNamespaces) (cfg:TypeProviderConfig) =
    let missingValue = "@@@missingValue###"
    let jsonDocumentType = erasedType<obj> thisAssembly rootNamespace "JsonZipper"
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
                    

                let parserType = erasedType<JsonZipper> thisAssembly rootNamespace typeName
     
                let defaultConstructor = 
                    ProvidedConstructor(
                        parameters = [],
                        InvokeCode = (fun args -> <@@ parse schema |> toZipper @@>))
                defaultConstructor.AddXmlDoc "Initializes the document from the schema sample."

                parserType.AddMember defaultConstructor

                let fileNameConstructor = 
                    ProvidedConstructor(
                        parameters = [ProvidedParameter("filename", typeof<string>)],
                        InvokeCode = (fun args -> <@@ (%%args.[0] : string) |> File.ReadAllText |> parse |> toZipper  @@>))
                fileNameConstructor.AddXmlDoc "Initializes a document from the given path."

                parserType.AddMember fileNameConstructor

                let inlinedDocumentConstructor = 
                    ProvidedConstructor(
                        parameters = [ProvidedParameter("documentContent", typeof<string>)],
                        InvokeCode = (fun args -> <@@ (%%args.[0] : string) |> parse |> toZipper @@>))
                inlinedDocumentConstructor.AddXmlDoc "Initializes a document from the given string."

                parserType.AddMember inlinedDocumentConstructor

                let toStringMethod =
                    ProvidedMethod(
                        methodName = "ToString",
                        parameters = [],
                        returnType = typeof<string>,
                        InvokeCode = (fun args -> <@@ (fromZipper (%%args.[0]: JsonZipper)).ToString() @@>))

                toStringMethod.AddXmlDoc "Gets the string representation"
                parserType.AddMember toStringMethod

                let createProperty parentType name propertyType = 
                    let newType = generateType parentType name

                    let property =
                        ProvidedProperty(
                            propertyName = niceName name,
                            propertyType = newType,
                            GetterCode = (fun args -> <@@ (%%args.[0]: JsonZipper) |> toProperty name @@>))

                    property.AddXmlDoc (sprintf "Gets the property named \"%s\"" name)

                    let toStringMethod =
                        ProvidedMethod(
                            methodName = "ToString",
                            parameters = [],
                            returnType = typeof<string>,
                            InvokeCode = (fun args -> <@@ (fromZipper (%%args.[0]: JsonZipper)).ToString() @@>))

                    toStringMethod.AddXmlDoc "Gets the string representation"

                    newType.AddMember toStringMethod
                    
                    let updateF =
                        ProvidedMethod(
                            methodName = "Update",
                            parameters = [ProvidedParameter("newValue",propertyType)],
                            returnType = newType,
                            InvokeCode =
                                (match propertyType with
                                 | x when x = typeof<int> -> fun args -> <@@ (%%args.[0]: JsonZipper) |> update (JsonValue.NumDecimal (decimal (%%args.[1]: int))) @@>
                                 | x when x = typeof<string> -> fun args -> <@@ (%%args.[0]: JsonZipper) |> update (JsonValue.String (%%args.[1]: string)) @@>))

                    updateF.AddXmlDoc (sprintf "Updates the value of the property named \"%s\"" name)

                    newType.AddMember updateF

                    let upF =
                        ProvidedMethod(
                            methodName = "Up",
                            parameters = [],
                            returnType = parentType,
                            InvokeCode = (fun args -> <@@ (%%args.[0]: JsonZipper)  @@>))

                    upF.AddXmlDoc "Moves the zipper one level up"

                    newType.AddMember upF

                    parentType.AddMember property

                let schema = JSONInference.provideElement "Document" false [parse schema]

                let generateProperties (CompoundProperty(elementName,multi,elementChildren,elementProperties)) =
                    for (SimpleProperty(propertyName,propertyType,optional)) in elementProperties do
                        createProperty parserType propertyName propertyType

                generateProperties schema

                let converterMethod =
                    ProvidedMethod(
                        methodName = "ToXml",
                        parameters = [],
                        returnType = typeof<XObject seq>,
                        InvokeCode = (fun args -> <@@ (fromZipper (%%args.[0]: JsonZipper)).ToXml() @@>))

                converterMethod.AddXmlDoc "Gets the XML representation"

                parserType.AddMember converterMethod
                parserType))
    jsonDocumentType