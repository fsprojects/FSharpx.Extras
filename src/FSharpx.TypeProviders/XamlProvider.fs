module FSharpx.TypeProviders.XamlProvider

open Samples.FSharpPreviewRelease2011.ProvidedTypes
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

let badargs() = failwith "Wrong type or number of arguments"

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
    { NodeType : Type
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
    |  "Window" -> typeof<System.Windows.Window>
    | other ->
        match wpfAssembly.GetType(sprintf "System.Windows.Controls.%s" other) with
        | null -> typeof<obj>
        | st -> st
   
let readXamlFile filename (xaml:XmlReader) =
    let rec readNewElement skipUnnamed siblings =
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
                        { NodeType = typeOfXamlElement typeName ;
                          Data = data ;
                          Children =
                            (if hasChildren then
                                readNewElement true []
                             else
                                []) }
                        :: siblings
                        |> readNewElement skipUnnamed
                    | true, Unnamed _ ->
                        readNewElement true siblings
                        |> readNewElement true
                | None -> failwithf "Error near %A" (posOfReader filename xaml)
            | XmlNodeType.EndElement ->
                siblings
            | XmlNodeType.Comment | XmlNodeType.Text -> readNewElement skipUnnamed siblings
            | unexpected -> failwithf "Unexpected node type %A at %A" unexpected (posOfReader filename xaml)

    let dontSkipTop = false
    match readNewElement dontSkipTop [] with
    | [root] -> root
    | _ :: _ -> failwith "Multiple roots"
    | [] -> failwith "No root"


let createXmlReader(textReader:TextReader) =
    XmlReader.Create(textReader, XmlReaderSettings(IgnoreProcessingInstructions = true, IgnoreWhitespace = true))

let rec createNestedType parent node =
    let mkTypeName = sprintf "GraphOf%s"
    let name =
        match node.Data.Name with
        | Some name -> name            
        | None ->
            failwith "Cannot create a nested type without a name"
    
    let nestedType =
        runtimeType<obj> (mkTypeName name)
        |+!> provideConstructor
                [("control", node.NodeType)]
                (function
                 | [x] -> x
                 | _ -> badargs())            
        |+> fun () ->
            provideProperty
                "Control"
                node.NodeType
                (function
                 | [this] -> Expr.Coerce(<@@ (%%this : obj) @@>, node.NodeType)
                 | _ -> badargs())
            |> addXmlDoc (sprintf "Access to the underlying %s" name)

    node.Children
    |> List.iter (createNestedType nestedType)

    parent
    |+!> nestedType
    |+> fun () ->
        provideProperty
            name
            nestedType
            (function
             | [this] -> Expr.Coerce(<@@ ((%%this : obj) :?> FrameworkElement).FindName(name) @@>, nestedType)
             | _ -> badargs())
        |> addDefinitionLocation node.Data.Position
    |> ignore

let createTypeFromReader typeName (xamlInfo:XamlInfo) (reader: TextReader) =
    let root = 
        createXmlReader(reader) 
        |> readXamlFile xamlInfo.FileName

    let rec checkConflictingNames root =
        let dups =
            root.Children
            |> Seq.groupBy (fun node -> node.Data.Name)
            |> Seq.tryFind (fun (key, values) -> Seq.length values > 1)
        match dups with
        | Some (_, nodes) ->
            failwithf
                "Components at %A under %A at %A have identical names"
                (nodes
                 |> Seq.map (fun node -> node.Data.Position)
                 |> List.ofSeq)
                (root.Data.Name)
                (root.Data.Position)
        | None ->
            root.Children
            |> Seq.iter checkConflictingNames

    checkConflictingNames root

    let topType =
        eraseType thisAssembly rootNamespace typeName typeof<obj>
            |> addDefinitionLocation root.Data.Position
            |+!> provideConstructor
                    [] 
                    (fun args -> 
                        match xamlInfo with 
                        | XamlInfo.Path path -> <@@ XamlReader.Parse(File.ReadAllText(path)) @@>
                        | XamlInfo.Text text -> <@@ XamlReader.Parse(text) @@>)
                |> addXmlDoc (sprintf "Initializes a wrapper over %s from Xaml" typeName)
                |> addDefinitionLocation root.Data.Position
            |+> fun () ->
                provideProperty
                    "Control"
                    root.NodeType
                    (function
                     | [this] -> Expr.Coerce(this, root.NodeType)
                     | _ -> badargs())
                |> addXmlDoc (sprintf "Access to the underlying %s" typeName)

    for child in root.Children do
        createNestedType topType child

    topType
        
let xamlFileTypeUninstantiated invalidateF (cfg:TypeProviderConfig) =
    erasedType<obj> thisAssembly rootNamespace "XamlFile"
      |> staticParameter "File" (fun typeName configFileName -> 
            let path = findConfigFile cfg.ResolutionFolder configFileName

            if File.Exists path |> not then
                failwithf "the path '%s' does not exist" path

            watchPath invalidateF path
            
            use reader = new StreamReader(path)
            createTypeFromReader typeName (XamlInfo.Path path) reader)

let xamlTextTypeUninstantiated (cfg:TypeProviderConfig) =
    erasedType<obj> thisAssembly rootNamespace "XamlText"
      |> staticParameter "File" (fun typeName text -> 
            use reader = new StringReader(text)
            createTypeFromReader typeName (XamlInfo.Text text) reader)