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
open FSharpx
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

[<RequireQualifiedAccess>]
type XamlInfo = 
| Path of string
| Text of string 
    member this.FileName =
        match this with 
        | XamlInfo.Path p -> p
        | _ -> null
    
type XamlId =  
| Named of FilePosition * string
| Unnamed of FilePosition
    member this.Position =
        match this with
        | Named(pos, _) | Unnamed(pos) -> pos

    member this.Name =
        match this with
        | Named(_, name) -> Some name
        | Unnamed _ -> None

type XamlElementInfo =
| XamlElementInfo of XamlId * string

type Tree<'Data> =
    { Data : 'Data
      Children : Tree<'Data> list }

type XamlNode =
    { IsRoot: bool
      Name: string
      NodeType : Type
      Data : XamlId
      Children : XamlNode list }

let posOfReader filename (xaml:XmlReader) = 
    let lineInfo = xaml :> obj :?> IXmlLineInfo
    { Line = lineInfo.LineNumber
      Column = lineInfo.LinePosition
      FileName = filename }

let infoOfXamlElement filename (xaml:XmlReader) =
    try
        let pos = posOfReader filename xaml
        let name = 
            match xaml.GetAttribute("Name") with
            | name when name <> null -> Named (pos, name)
            | _ ->
                match xaml.GetAttribute("x:Name") with
                | name when name <> null -> Named (pos, name)
                | _ -> Unnamed (pos)
        
        XamlElementInfo(name,xaml.Name) |> Some
    with
    | :? XmlException -> None

let typeOfXamlElement = function 
| "Window" -> typeof<System.Windows.Window>
| other ->
    match wpfAssembly.GetType(sprintf "System.Windows.Controls.%s" other) with
    | null -> typeof<obj>
    | st -> st

   
let readXamlFile filename (xaml:XmlReader) =
    let rec readNewElement isRoot skipUnnamed siblings =
        match xaml.Read() with
        | false -> siblings
        | true ->
            match xaml.NodeType with
            | XmlNodeType.Element ->
                match infoOfXamlElement filename xaml with
                | Some (XamlElementInfo(data, typeName)) ->
                    let hasChildren = not xaml.IsEmptyElement
                    match skipUnnamed, data with
                    | _, Named _ | false, Unnamed _->
                        { IsRoot = isRoot
                          Name =
                            match data.Name with
                            | Some name -> name            
                            | None -> 
                                if isRoot then "Root" else
                                failwith "Cannot create a nested type without a name" // TODO: Generate one
                          NodeType = typeOfXamlElement typeName
                          Data = data 
                          Children = (if hasChildren then readNewElement false true [] else []) }
                        :: siblings
                        |> readNewElement false skipUnnamed
                    | true, Unnamed _ ->
                        readNewElement false true siblings
                        |> readNewElement false true
                | None -> failwithf "Error near %A" (posOfReader filename xaml)
            | XmlNodeType.EndElement -> siblings
            | XmlNodeType.Comment | XmlNodeType.Text -> readNewElement false skipUnnamed siblings
            | unexpected -> failwithf "Unexpected node type %A at %A" unexpected (posOfReader filename xaml)

    let dontSkipTop = false

    match readNewElement true dontSkipTop [] with
    | [root] -> root
    | _ :: _ -> failwith "Multiple roots"
    | [] -> failwith "No root"

let createXmlReader(textReader:TextReader) =
    XmlReader.Create(textReader, XmlReaderSettings(IgnoreProcessingInstructions = true, IgnoreWhitespace = true))

let rec getAllSubElementsWithName (node:XamlNode) =      
    seq { yield node
          for child in node.Children do
            yield! getAllSubElementsWithName child }

let createTypeFromReader typeName (xamlInfo:XamlInfo) (reader: TextReader) =
    let root = 
        reader
        |> createXmlReader 
        |> readXamlFile xamlInfo.FileName

    let subElements = getAllSubElementsWithName root

    let accessExpr node (args:Expr list) =
        let name = node.Name
        let expr = if node.IsRoot then <@@ (%%args.[0] :> XamlFile).Root @@> else <@@ (%%args.[0] :> XamlFile).GetChild name @@>
        Expr.Coerce(expr,node.NodeType)

    erasedType<XamlFile> thisAssembly rootNamespace typeName
        |> addDefinitionLocation root.Data.Position
        |+!> (provideConstructor
                [] 
                (fun args -> 
                    match xamlInfo with 
                    | XamlInfo.Path path -> <@@ XamlFile(XamlReader.Parse(File.ReadAllText(path)) :?> FrameworkElement) @@>
                    | XamlInfo.Text text -> <@@ XamlFile(XamlReader.Parse(text) :?> FrameworkElement) @@>)
                |> addXmlDoc (sprintf "Initializes typed access to %s" xamlInfo.FileName)
                |> addDefinitionLocation root.Data.Position)
    |++!> (
        subElements
        |> Seq.map (fun node ->
             provideProperty node.Name node.NodeType (accessExpr node)
             |> addXmlDoc (sprintf "Gets the %s element" node.Name)
             |> addDefinitionLocation node.Data.Position))   

/// Infer schema from the loaded data and generate type with properties     
let xamlType (ownerType:TypeProviderForNamespaces)  (cfg:TypeProviderConfig) =
    erasedType<obj> thisAssembly rootNamespace "XAML"
      |> staticParameter "FileName" (fun typeName configFileName -> 
            let path = findConfigFile cfg.ResolutionFolder configFileName

            if File.Exists path |> not then
                failwithf "the path '%s' does not exist" path
            
            watchForChanges ownerType path
            
            use reader = new StreamReader(path)
            createTypeFromReader typeName (XamlInfo.Path path) reader)