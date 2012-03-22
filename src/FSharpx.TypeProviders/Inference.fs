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

/// Generate property for every inferred property
let generateProperties ownerType accessExpr checkIfOptional elementProperties =   
    for SimpleProperty(propertyName,propertyType,optional) in elementProperties do
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
                          (checkIfOptional propertyName args,
                            Expr.NewUnionCase(some, [accessExpr propertyName propertyType args]),
                            Expr.NewUnionCase(none, [])))
            else
                provideProperty 
                    propertyName
                    propertyType
                    (accessExpr propertyName propertyType)
        
        ownerType
          |+!> (prop |> addXmlDoc (sprintf "Gets the %s attribute" propertyName))
          |> ignore

let generateSublements ownerType parentType multiAccessExpr singleAccessExpr generateTypeF children =
    // Iterate over all the JSON sub elements, generate type for them
    // and add member for accessing them to the parent.
    for CompoundProperty(childName,multi,_,_) as child in children do
        let childType = generateTypeF parentType child

        if multi then     
            let newType = seqType childType

            ownerType
            |+!> (provideMethod
                    ("Get" + niceName childName + "Elements")
                    []
                    newType
                    (multiAccessExpr childName)
                    |> addXmlDoc (sprintf @"Gets the %s elements" childName))
            |> ignore               
        else
            provideProperty 
                childName
                childType
                (singleAccessExpr childName)

            |> addXmlDoc (sprintf @"Gets the %s attribute" childName)
            |> ownerType.AddMember

    ownerType


let createParserType<'a> typeName schema (generateTypeF: ProvidedTypeDefinition -> CompoundProperty -> ProvidedTypeDefinition)
                           emptyConstructor fileNameConstructor rootPropertyGetter =
    // -------------------------------------------------------------------------------------------
    // Generate constructors for loading data and add type representing Root node        
        
    let parserType = erasedType<'a> thisAssembly rootNamespace typeName
    parserType
    |+!> (provideConstructor
            []
            emptyConstructor
        |> addXmlDoc "Initializes the document from the schema sample.")
    |+!> (provideConstructor
            ["filename", typeof<string>] 
            fileNameConstructor
        |> addXmlDoc "Initializes a document from the given path.")
    |+!> provideProperty
            "Root"
            (generateTypeF parserType schema)
            rootPropertyGetter


let createStructuredParser typeName resolutionFolder ownerType createTypeF =
    erasedType<obj> thisAssembly rootNamespace typeName
    |> staticParameters 
          ["FileName" , typeof<string>, Some("@@@missingValue###" :> obj)  // Parameterize the type by the file to use as a template
           "Schema" , typeof<string>, Some("@@@missingValue###" :> obj)  ]    // Allows to specify inlined schema
          (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as fileName; :? string |] when fileName <> "@@@missingValue###" ->        
                let resolvedFileName = findConfigFile resolutionFolder fileName
                watchForChanges ownerType resolvedFileName

                let schema = File.ReadAllText resolvedFileName
                createTypeF typeName schema
            | [| :? string; :? string as schema |] when schema <> "@@@missingValue###" ->        
                createTypeF typeName schema
            | _ -> failwith "You have to specify a filename or inlined Schema")