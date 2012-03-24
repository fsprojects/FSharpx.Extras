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
open FSharpx.TypeProviders.DSL
open FSharpx.TypeProviders.Settings

let wpfAssembly = typeof<System.Windows.Controls.Button>.Assembly

/// Simple type wrapping Xaml file
type XamlFile(root:FrameworkElement) =
    let dict = new Dictionary<_,_>()

    member this.GetChild name = 
        match dict.TryGetValue name with
        | true,element -> element
        | false,element -> 
            let element = root.FindName name
            dict.[name] <- element
            element

    member this.Root = root

type XamlNode =
    { Position: FilePosition
      IsRoot: bool
      Name: string
      NodeType : Type }

let posOfReader filename (xaml:XmlReader) = 
    let lineInfo = xaml :> obj :?> IXmlLineInfo
    { Line = lineInfo.LineNumber
      Column = lineInfo.LinePosition
      FileName = filename }

let createXamlNode filename isRoot (xaml:XmlReader) =
    let pos = posOfReader filename xaml
    try
        let typeName = xaml.Name
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
                match typeName with
                | "Window" -> typeof<Window>
                | other ->
                    match wpfAssembly.GetType(sprintf "System.Windows.Controls.%s" other) with
                    | null -> typeof<obj>
                    | st -> st

            { Position = pos
              IsRoot = isRoot
              Name = name
              NodeType = propertyType }
            |> Some
    with
    | :? XmlException -> failwithf "Error near %A" pos

let readXamlFile filename (xaml:XmlReader) =    
    seq {
        let isRoot = ref true
        while xaml.Read() do
            match xaml.NodeType with
            | XmlNodeType.Element ->               
                match createXamlNode filename (!isRoot) xaml with
                | Some node ->
                    yield node
                    isRoot := false
                | None -> ()
            | XmlNodeType.EndElement | XmlNodeType.Comment | XmlNodeType.Text -> ()
            | unexpected -> failwithf "Unexpected node type %A at %A" unexpected (posOfReader filename xaml) }

let createXmlReader(textReader:TextReader) =
    XmlReader.Create(textReader, XmlReaderSettings(IgnoreProcessingInstructions = true, IgnoreWhitespace = true))

let createTypeFromReader typeName fileName schema (reader: TextReader) =
    let elements = 
        reader
        |> createXmlReader 
        |> readXamlFile fileName
        |> Seq.toList

    let root = List.head elements

    let accessExpr node (args:Expr list) =
        let name = node.Name
        let expr = if node.IsRoot then <@@ (%%args.[0] :> XamlFile).Root @@> else <@@ (%%args.[0] :> XamlFile).GetChild name @@>
        Expr.Coerce(expr,node.NodeType)

    erasedType<XamlFile> thisAssembly rootNamespace typeName
        |> addDefinitionLocation root.Position
        |+!> (provideConstructor
                [] 
                (fun args -> <@@ XamlFile(XamlReader.Parse(schema) :?> FrameworkElement) @@>)
                |> addXmlDoc (sprintf "Initializes typed access to %s" fileName)
                |> addDefinitionLocation root.Position)
    |++!> (
        elements
        |> Seq.map (fun node ->
             provideProperty node.Name node.NodeType (accessExpr node)
             |> addXmlDoc (sprintf "Gets the %s element" node.Name)
             |> addDefinitionLocation node.Position))   

/// Infer schema from the loaded data and generate type with properties     
let xamlType (ownerType:TypeProviderForNamespaces)  (cfg:TypeProviderConfig) =
    let createTypeFromFileName typeName (fileName:string) =        
        use reader = new StreamReader(fileName)
        createTypeFromReader typeName fileName (File.ReadAllText fileName) reader

    let createTypeFromSchema typeName (schema:string) =        
        use reader = new StringReader(schema)
        createTypeFromReader typeName null schema reader
    
    createStructuredParser thisAssembly rootNamespace "XAML" cfg ownerType createTypeFromFileName createTypeFromSchema