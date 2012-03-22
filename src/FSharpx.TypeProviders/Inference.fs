// ----------------------------------------------------------------------------
// Original Xml type provider
// (c) Tomas Petricek - tomasP.net, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
module FSharpx.TypeProviders.Inference

open System
open System.Xml.Linq
open FSharpx.TypeProviders.DSL
open System.Collections.Generic
open System.Globalization

/// Checks whether the string is a boolean value
let isBool (s:string) =
    let l = s.ToLower()
    l = "true" || l = "false" || l = "yes" || l = "no"

/// Checks whether the string is an int
let isInt (s:string) = Int32.TryParse s |> fst

/// Checks whether the string is a float
let isFloat (s:string) =
      Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) 
        |> fst

/// Checks whether all values of the sequence can be inferred to a special type
let inferType values =     
    if Seq.forall isBool values then typeof<bool>
    elif Seq.forall isInt values then typeof<int>
    elif Seq.forall isFloat values then typeof<float>
    else typeof<string>

// ------------------------------------------------------------------------------------------------
// Representation about inferred structure
// ------------------------------------------------------------------------------------------------

type SimpleProperty = SimpleProperty of string * Type * bool

type CompoundProperty = CompoundProperty of string * bool * CompoundProperty seq * SimpleProperty seq

open Settings
open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices

/// Generate property for every inferred property
let generateProperties ownerType accessExpr checkIfOptional elementProperties =   
    for SimpleProperty(propertyName,propertyType,optional) in elementProperties do
        ownerType
          |+!> (if optional then
                    let newType = optionType propertyType
                    // For optional elements, we return Option value
                    let cases = Reflection.FSharpType.GetUnionCases newType
                    let some = cases |> Seq.find (fun c -> c.Name = "Some")
                    let none = cases |> Seq.find (fun c -> c.Name = "None")

                    let optionalAccessExpr =
                        (fun args ->
                            Expr.IfThenElse
                                (checkIfOptional propertyName args,
                                Expr.NewUnionCase(some, [accessExpr propertyName propertyType args]),
                                Expr.NewUnionCase(none, [])))

                    provideProperty propertyName newType optionalAccessExpr                    
                else
                    provideProperty propertyName propertyType (accessExpr propertyName propertyType)
                |> addXmlDoc (sprintf "Gets the %s attribute" propertyName))
          |> ignore

/// Iterates over all the sub elements, generates types for them
/// and adds member for accessing them to the parent.
let generateSublements ownerType parentType multiAccessExpr singleAccessExpr generateTypeF children =
    for CompoundProperty(childName,multi,_,_) as child in children do
        let childType = generateTypeF parentType child

        if multi then     
            let newType = seqType childType
            let methodName = "Get" + niceName childName + "Elements"

            ownerType
            |+!> (provideMethod methodName [] newType (multiAccessExpr childName)
                    |> addXmlDoc (sprintf @"Gets the %s elements" childName))
            |> ignore               
        else
            ownerType
            |+!> (provideProperty childName childType (singleAccessExpr childName)
                    |> addXmlDoc (sprintf @"Gets the %s attribute" childName))
            |> ignore

    ownerType

/// Generates constructors for loading data and adds type representing Root node
let createParserType<'a> typeName schema (generateTypeF: ProvidedTypeDefinition -> CompoundProperty -> ProvidedTypeDefinition)
         emptyConstructor fileNameConstructor rootPropertyGetter =
    let parserType = erasedType<'a> thisAssembly rootNamespace typeName
    parserType
    |+!> (provideConstructor [] emptyConstructor
        |> addXmlDoc "Initializes the document from the schema sample.")
    |+!> (provideConstructor ["filename", typeof<string>] fileNameConstructor
        |> addXmlDoc "Initializes a document from the given path.")
    |+!> provideProperty "Root" (generateTypeF parserType schema) rootPropertyGetter


/// Generates a structured parser
let createStructuredParser typeName (cfg:TypeProviderConfig) ownerType createTypeF =
    let missingValue = "@@@missingValue###"
    erasedType<obj> thisAssembly rootNamespace typeName
    |> staticParameters 
          ["FileName" , typeof<string>, Some(missingValue :> obj)  // Parameterize the type by the file to use as a template
           "Schema" , typeof<string>, Some(missingValue :> obj)  ] // Allows to specify inlined schema
          (fun typeName parameterValues ->
                match parameterValues with 
                | [| :? string as fileName; :? string |] when fileName <> missingValue ->        
                    let resolvedFileName = findConfigFile cfg.ResolutionFolder fileName
                    watchForChanges ownerType resolvedFileName
                
                    createTypeF typeName <| File.ReadAllText resolvedFileName
                | [| :? string; :? string as schema |] when schema <> missingValue ->        
                    createTypeF typeName schema
                | _ -> failwith "You have to specify a filename or inlined Schema")