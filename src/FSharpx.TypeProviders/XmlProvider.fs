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

// ------------------------------------------------------------------------------------------------
// Type provider
// ------------------------------------------------------------------------------------------------
    
let xmlType (ownerType:TypeProviderForNamespaces) (cfg:TypeProviderConfig) =
  // Implements invalidation of schema when the file changes
  let watchForChanges  (fileName:string) = 
    if not (fileName.StartsWith("http", StringComparison.InvariantCultureIgnoreCase)) then
      let path = Path.GetDirectoryName(fileName)
      let name = Path.GetFileName(fileName)
      let watcher = new FileSystemWatcher(Filter = name, Path = path)
      watcher.Changed.Add(fun _ -> ownerType.Invalidate()) 
      watcher.EnableRaisingEvents <- true
  
  // Create the main provided type
  let xmlType = ProvidedTypeDefinition(thisAssembly, rootNamespace, "StructuredXml", Some(typeof<obj>))

  // Parameterize the type by the file to use as a template
  let parameter = ProvidedStaticParameter("FileName", typeof<string>)
  do xmlType.DefineStaticParameters([parameter], fun typeName args ->

    // Resolve the filename relative to the resolution folder
    let fileName = args.[0] :?> string
    let resolvedFilename = 
      if not (fileName.StartsWith("http", StringComparison.InvariantCultureIgnoreCase)) then
        Path.Combine(cfg.ResolutionFolder, fileName)
      else fileName
    let doc = XDocument.Load(resolvedFilename)
    watchForChanges resolvedFilename

    // -------------------------------------------------------------------------------------------
    // Infer schema from the loaded data and generate type with properties

    let schema = Inference.provideElement doc.Root.Name.LocalName [doc.Root]      
    let resTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeName, Some typeof<TypedXDocument>)


    // Generates type for an inferred XML element
    let rec generateType (ProvidedXElement(name, niceName, children, attributes)) =
      let ty = ProvidedTypeDefinition(makeUniqueName niceName, Some(typeof<TypedXElement>))
      resTy.AddMember(ty)

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
        let chty = generateType child

        ProvidedMethod("Get" + niceName + "Elements", [], seqType chty, InvokeCode = fun [self] ->
          <@@ seq { for e in ((%%self:TypedXElement).Element.Elements(XName.op_Implicit name)) -> TypedXElement(e) } @@>)
        |> ty.AddMember

      ty

    // -------------------------------------------------------------------------------------------
    // Generate constructors for loading XML data and add type representing Root node

    ProvidedConstructor([], InvokeCode = fun _ -> 
      <@@ TypedXDocument(XDocument.Load(resolvedFilename)) @@>)
    |> resTy.AddMember

    ProvidedConstructor([ProvidedParameter("filename", typeof<string>)], InvokeCode = fun [filename] -> 
      <@@ TypedXDocument(XDocument.Load(%%filename : string)) @@>)
    |> resTy.AddMember
    
    let rootTy = generateType schema 
    ProvidedProperty("Root", rootTy, GetterCode = fun [self] ->
      <@@ TypedXElement((%%self : TypedXDocument).Document.Root) @@>)
    |> resTy.AddMember

    resTy)
  xmlType