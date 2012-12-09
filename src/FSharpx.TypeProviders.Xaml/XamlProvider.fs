module FSharpx.TypeProviders.XamlProvider

open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Windows
open System.Windows.Markup
open System.Xml
open System.Linq.Expressions
open FSharpx.TypeProviders.Helper

let private wpfAssembly = typeof<System.Windows.Controls.Button>.Assembly

/// Simple type wrapping Xaml file
type XamlFile(schema: string) =
    let dict = new Dictionary<_,_>()
    let root = XamlReader.Parse(schema) :?> FrameworkElement

    member this.GetChild name = 
        match dict.TryGetValue name with
        | true,element -> element
        | false,element -> 
            let element = root.FindName name
            dict.[name] <- element
            element

    member this.Root = root

type internal FilePosition =  
   { Line: int; 
     Column: int;
     FileName: string }

type internal XamlNode =
    { Position: FilePosition
      IsRoot: bool
      Name: string
      NodeType : Type }

let internal posOfReader filename (xaml:XmlReader) = 
    let lineInfo = xaml :> obj :?> IXmlLineInfo
    { Line = lineInfo.LineNumber
      Column = lineInfo.LinePosition
      FileName = filename }

let internal createXamlNode (schemaContext: Xaml.XamlSchemaContext) filename isRoot (xaml:XmlReader) =
    let pos = posOfReader filename xaml
    try 
        let name =                        
            match xaml.GetAttribute("Name") with
            | name when name <> null -> Some name
            | _ ->
                match xaml.GetAttribute("x:Name") with
                | name when name <> null -> Some name
                | _ -> if isRoot then Some "Root" else None

        match name with
        | None -> None
        | Some name -> 
            let propertyType =
                let r = schemaContext.GetAllXamlTypes(xaml.NamespaceURI)
                let xamltype = r |> Seq.tryFind (fun xt -> xt.Name = xaml.LocalName)
                match xamltype with
                | None   -> typeof<obj>
                | Some t -> t.UnderlyingType
            { Position = pos
              IsRoot = isRoot
              Name = name
              NodeType = propertyType }
            |> Some
    with
    | :? XmlException -> failwithf "Error near %A" pos

let internal readXamlFile (schemaContext: Xaml.XamlSchemaContext) filename (xaml:XmlReader) =    
    seq {
        let isRoot = ref true
        while xaml.Read() do
            match xaml.NodeType with
            | XmlNodeType.Element ->               
                match createXamlNode schemaContext filename (!isRoot) xaml with
                | Some node ->
                    yield node
                    isRoot := false
                | None -> ()
            | XmlNodeType.EndElement | XmlNodeType.Comment | XmlNodeType.Text -> ()
            | unexpected -> failwithf "Unexpected node type %A at %A" unexpected (posOfReader filename xaml) }

let createXmlReader(textReader:TextReader) =
    XmlReader.Create(textReader, XmlReaderSettings(IgnoreProcessingInstructions = true, IgnoreWhitespace = true))

let internal createTypeFromReader (schemaContext: Xaml.XamlSchemaContext) typeName fileName schema (reader: TextReader) =
    let elements = 
        reader
        |> createXmlReader 
        |> readXamlFile schemaContext fileName
        |> Seq.toList

    let root = List.head elements

    let accessExpr node (args:Expr list) =
        let name = node.Name
        let expr = if node.IsRoot then <@@ (%%args.[0] :> XamlFile).Root @@> else <@@ (%%args.[0] :> XamlFile).GetChild name @@>
        Expr.Coerce(expr,node.NodeType)

    let xamlType = erasedType<XamlFile> thisAssembly rootNamespace typeName
    xamlType.AddDefinitionLocation(root.Position.Line,root.Position.Column,root.Position.FileName)

    let ctor = 
        ProvidedConstructor(
            parameters = [], 
            InvokeCode = (fun args -> <@@ XamlFile(schema) @@>))

    ctor.AddXmlDoc (sprintf "Initializes typed access to %s" fileName)
    ctor.AddDefinitionLocation(root.Position.Line,root.Position.Column,root.Position.FileName)
    xamlType.AddMember ctor

    for node in elements do
        let property = 
            ProvidedProperty(
                propertyName = node.Name,
                propertyType = node.NodeType,
                GetterCode = accessExpr node)
        property.AddXmlDoc(sprintf "Gets the %s element" node.Name)
        property.AddDefinitionLocation(root.Position.Line,root.Position.Column,root.Position.FileName)
        xamlType.AddMember property
   
    xamlType 

/// Infer schema from the loaded data and generate type with properties     
let internal xamlType (ownerType:TypeProviderForNamespaces)  (cfg:TypeProviderConfig) =

    cfg.ReferencedAssemblies 
    |> Seq.map (fun a -> System.IO.FileInfo(a).DirectoryName)
    |> Set.ofSeq
    |> Set.iter (fun p -> ownerType.RegisterProbingFolder p)
   
    let assemblies = 
        cfg.ReferencedAssemblies 
        |> Seq.map (fun r -> Assembly.Load(IO.File.ReadAllBytes r))
        |> Seq.append [wpfAssembly]
        |> Array.ofSeq
    let ss = Xaml.XamlSchemaContextSettings()
    ss.FullyQualifyAssemblyNamesInClrNamespaces <- false
    ss.SupportMarkupExtensionsWithDuplicateArity <- false
    let schemaContext = System.Xaml.XamlSchemaContext(assemblies, ss)//  (assemblies)

    let createTypeFromFileName typeName (fileName:string) =        
        use reader = new StreamReader(fileName)
        createTypeFromReader schemaContext typeName fileName (File.ReadAllText fileName) reader

    let createTypeFromSchema typeName (schema:string) =        
        use reader = new StringReader(schema)
        createTypeFromReader schemaContext typeName null schema reader

    let xamlType = erasedType<obj> thisAssembly rootNamespace "XAML"
    xamlType.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("FileName", typeof<string>, missingValue) // Parameterize the type by the file to use as a template
                      ProvidedStaticParameter("Schema", typeof<string>, missingValue)], // Allows to specify inlined schema
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as fileName; :? string |] when fileName <> missingValue ->        
                let resolvedFileName = findConfigFile cfg.ResolutionFolder fileName
                watchForChanges ownerType resolvedFileName
                
                createTypeFromFileName typeName resolvedFileName
            | [| :? string; :? string as schema |] when schema <> missingValue ->        
                createTypeFromSchema typeName schema
            | _ -> failwith "You have to specify a filename or inlined Schema"))

    xamlType

[<TypeProvider>]
type public XamlProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(rootNamespace,[xamlType this cfg])

[<TypeProviderAssembly>]
do ()