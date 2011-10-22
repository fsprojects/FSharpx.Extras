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

type XamlElementInfo =
| XamlElementInfo of XamlId * string

type XamlNode =
    { NodeType : Type
      Data : XamlId
      Children : XamlNode list }

(*
type XamlFile = 
    { Root : Type
      Position : FilePosition
      Children : list<XamlId * Type> }   
    static member FileStart fileName = 
        { Root = typeof<obj>
          Position = fileStart fileName
          Children = [] }
*)

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
    let rec readNewElement siblings =
        match xaml.Read() with
        | false -> siblings
        | true ->
            match xaml.NodeType with
            | XmlNodeType.Element ->
                match infoOfXamlElement filename xaml with
                | Some (XamlElementInfo(data, typeName)) ->
                    let has_children = not xaml.IsEmptyElement
                    { NodeType = typeOfXamlElement typeName ;
                      Data = data ;
                      Children =
                        (if has_children then
                            readNewElement []
                         else
                            []) }
                    :: siblings
                    |> readNewElement
                | None -> failwithf "Error near %A" (posOfReader filename xaml)
            | XmlNodeType.EndElement ->
                siblings
            | XmlNodeType.Comment | XmlNodeType.Text -> readNewElement siblings
            | unexpected -> failwithf "Unexpected node type %A at %A" unexpected (posOfReader filename xaml)

    match readNewElement [] with
    | [root] -> root
    | _ :: _ -> failwith "Multiple roots"
    | [] -> failwith "No root"


let createXmlReader(textReader:TextReader) =
    XmlReader.Create(textReader, XmlReaderSettings(IgnoreProcessingInstructions = true, IgnoreWhitespace = true))

let createTypeFromReader typeName (xamlInfo:XamlInfo) (reader: TextReader) =
    let root = 
        createXmlReader(reader) 
        |> readXamlFile xamlInfo.FileName

    eraseType thisAssembly rootNamespace typeName root.NodeType
        |> addDefinitionLocation root.Data.Position
        |+> (provideConstructor
                [] 
                (fun args -> 
                    match xamlInfo with 
                    | XamlInfo.Path path -> Expr.Coerce(<@@ XamlReader.Parse(File.ReadAllText(path)) @@>, root.NodeType)
                    | XamlInfo.Text text -> Expr.Coerce(<@@ XamlReader.Parse(text) @@>, root.NodeType))
            |> addXmlDoc (sprintf "Initializes a %s instance" typeName)
            |> addDefinitionLocation root.Data.Position)

    //TODO: Lift children of unnamed nodes
    //TODO: Generate nested types for each child
    //TODO: Generate properties for each child
(*        |++> (root.Children
                |> List.map 
                    (fun (XamlId(pos, propertyName), resultType) ->
                        provideProperty
                            propertyName
                            resultType
                            (fun args -> Expr.Coerce(<@@ (%%args.[0]:Window).FindName(propertyName) @@>, resultType))
                        |> addDefinitionLocation pos)) *)
        
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