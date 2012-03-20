module FSharpx.TypeProviders.JsonTypeProvider

open System
open System.IO
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Inference
open FSharpx.TypeProviders.JSONParser

let toString json =
    match json with
    | Text t -> t
    | Number n -> n.ToString()
    | Boolean b -> b.ToString()


// Generates type for an inferred XML element
let rec generateType (ownerType:ProvidedTypeDefinition) (CompoundProperty(elementName,multiProperty,elementChildren,elementProperties)) =
    let ty = runtimeType<JSON> elementName
    ownerType.AddMember(ty)
       
    // Generate property for every inferred property
    for SimpleProperty(propertyName,propertyType,optional) in elementProperties do
        let accessExpr (args: Expr list) = 
            match propertyType with
            | x when x = typeof<string> -> 
                <@@ (%%args.[0]:JSON).GetProperty(propertyName).GetText() @@>
            | x when x = typeof<bool> -> 
                <@@ (%%args.[0]:JSON).GetProperty(propertyName).GetBoolean() @@>
            | x when x = typeof<int> -> 
                <@@ (%%args.[0]:JSON).GetProperty(propertyName).GetNumber() |> int @@>
            | x when x = typeof<float> -> 
                <@@ (%%args.[0]:JSON).GetProperty(propertyName).GetNumber() @@>

        let prop =
            if optional then
                failwith "Not implemented"
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
                          (<@@ (%%args.[0]:JSON).HasProperty propertyName @@>,
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

    // Iterate over all the JSON sub elements, generate type for them
    // and add member for accessing them to the parent.
    for CompoundProperty(childName,multi,_,_) as child in elementChildren do
        let childType = generateType ownerType child

        if not multi then            
            provideProperty 
                childName
                childType
                (fun args -> <@@ ((%%args.[0]:JSON).GetProperty childName) @@>)

            |> addXmlDoc (sprintf @"Gets the ""%s"" attribute" childName)
            |> ty.AddMember
        else
            let newType = seqType childType

            ty
            |+!> (provideMethod
                    ("Get" + niceName childName + "Elements")
                    []
                    newType
                    (fun args -> <@@ (%%args.[0]:JSON).GetProperty(childName).GetSubElements() @@>)
                    |> addXmlDoc (sprintf @"Gets the ""%s"" elements" childName))
            |> ignore
    ty

let jsonType (ownerType:TypeProviderForNamespaces) (cfg:TypeProviderConfig) =  
  erasedType<obj> thisAssembly rootNamespace "StructuredJSON"  
  |> staticParameter "FileName"  // Parameterize the type by the file to use as a template
      (fun typeName fileName ->        
        let resolvedFileName = findConfigFile cfg.ResolutionFolder fileName
        let doc = 
            resolvedFileName
            |> File.ReadAllText
            |> parse

        watchForChanges ownerType resolvedFileName

        // -------------------------------------------------------------------------------------------
        // Infer schema from the loaded data and generate type with properties

        let schema = JSONInference.provideElement "Document" false [doc]      
        let resTy = erasedType<JSON> thisAssembly rootNamespace typeName
       
        // -------------------------------------------------------------------------------------------
        // Generate constructors for loading Json data and add type representing Root node        
        resTy
        |+!> (provideConstructor
                [] 
                (fun args -> <@@ resolvedFileName |> File.ReadAllText |> parse @@>)
            |> addXmlDoc "Initializes the JSON document with the schema sample")
        |+!> (provideConstructor
                ["filename", typeof<string>] 
                (fun args -> <@@ (%%args.[0] : string) |> File.ReadAllText |> parse  @@>)
            |> addXmlDoc "Initializes a JSON document from the given path.")
        |+!> provideProperty
                "Root"
                (generateType resTy schema)
                (fun args -> <@@ (%%args.[0] : JSON) @@>))