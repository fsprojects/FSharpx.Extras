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
open System.Xml.Linq

// Generates type for an inferred XML element
let rec generateType (ownerType:ProvidedTypeDefinition) (ProvidedXElement(name, niceName, children, attributes)) =
    let ty = runtimeType<TypedXElement> (makeUniqueName niceName)
    ownerType.AddMember(ty)

    // Generate property for every inferred attribute of the element
    for ProvidedXAttribute(name, niceName, typ, opt) in attributes do 
        if opt then
            // For optional elements, we return Option value
            ProvidedProperty(niceName, optionType typ, GetterCode = fun [self] ->
                let accessExpr = 
                  <@@  (%%self:TypedXElement).Element.
                          Attribute(XName.op_Implicit name).Value @@> 
                  |> convertExpr typ 

                let cases = Reflection.FSharpType.GetUnionCases(optionType typ)
                let some = cases |> Seq.find (fun c -> c.Name = "Some")
                let none = cases |> Seq.find (fun c -> c.Name = "None")

                Expr.IfThenElse
                  ( <@@ (%%self:TypedXElement).Element.
                          Attribute(XName.op_Implicit name) <> null @@>,
                    Expr.NewUnionCase(some, [accessExpr]),
                    Expr.NewUnionCase(none, []) ) )
              |> ty.AddMember
        else
            ProvidedProperty(niceName, typ, GetterCode = fun [self] ->
                <@@ (%%self:TypedXElement).Element.
                      Attribute(XName.op_Implicit name).Value @@>
                |> convertExpr typ)
              |> ty.AddMember


    // Iterate over all the XML elements, generate type for them
    // and add member for accessing them to the parent.
    for (ProvidedXElement(name, niceName, _, _)) as child in children do
        let chty = generateType ownerType child

        ProvidedMethod("Get" + niceName + "Elements", [], seqType chty, InvokeCode = fun [self] ->
              <@@ seq { for e in ((%%self:TypedXElement).Element.Elements(XName.op_Implicit name)) -> TypedXElement(e) } @@>)
          |> ty.AddMember

    ty

let xmlType (ownerType:TypeProviderForNamespaces) (cfg:TypeProviderConfig) =  
  erasedType<obj> thisAssembly rootNamespace "StructuredXml"  
  |> staticParameter "FileName"  // Parameterize the type by the file to use as a template
      (fun typeName fileName ->
        let resolvedFileName = findConfigFile cfg.ResolutionFolder fileName
        let doc = XDocument.Load resolvedFileName
        watchForChanges ownerType resolvedFileName

        // -------------------------------------------------------------------------------------------
        // Infer schema from the loaded data and generate type with properties

        let schema = Inference.provideElement doc.Root.Name.LocalName [doc.Root]      
        let resTy = erasedType<TypedXDocument> thisAssembly rootNamespace typeName
       
        // -------------------------------------------------------------------------------------------
        // Generate constructors for loading XML data and add type representing Root node        
        resTy
        |+!> (provideConstructor
                [] 
                (fun args -> <@@ TypedXDocument(XDocument.Load resolvedFileName) @@>)
            |> addXmlDoc "Initializes the XML document with the schema sample")
        |+!> (provideConstructor
                ["filename", typeof<string>] 
                (fun args -> <@@ TypedXDocument(XDocument.Load(%%args.[0] : string)) @@>)
            |> addXmlDoc "Initializes a XML document from the given path.")
        |+!> provideProperty
                "Root"
                (generateType resTy schema)
                (fun args -> <@@ TypedXElement((%%args.[0] : TypedXDocument).Document.Root) @@>))