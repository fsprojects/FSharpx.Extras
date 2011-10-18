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
| XamlId of FilePosition * string

type XamlElementInfo =
| XamlElementInfo of XamlId option * string

type XamlFile = 
    { Root : Type
      Position : FilePosition
      Children : list<XamlId * Type> }   
    static member FileStart fileName = 
        { Root = typeof<obj>
          Position = fileStart fileName
          Children = [] }

let posOfReader info (xaml:XmlReader) = 
    let lineInfo = xaml :> obj :?> IXmlLineInfo
    { Line = lineInfo.LineNumber
      Column = lineInfo.LinePosition
      FileName = info.FileName }

let infoOfXamlElement info (xaml:XmlReader) =
    try
        let pos = posOfReader info xaml
        let name = 
            match xaml.GetAttribute("Name") with
            | name when name <> null -> XamlId (pos, name) |> Some
            | _ ->
                match xaml.GetAttribute("x:Name") with
                | name when name <> null -> XamlId (pos, name) |> Some
                | _ -> None        
        
        XamlElementInfo(name,xaml.Name) |> Some
    with
    | :? XmlException -> None

let typeOfXamlElement = function 
    |  "Window" -> typeof<System.Windows.Window>
    | other ->
        match wpfAssembly.GetType(sprintf "System.Windows.Controls.%s" other) with
        | null -> typeof<obj>
        | st -> st
   
let readXamlFile info (xaml:XmlReader) =
    let rec readElementChildren () = 
        seq {
            let more = ref true
            while !more && not xaml.EOF do
                match xaml.Read() with
                | false -> more := false
                | true -> 
                    match xaml.NodeType with
                    | XmlNodeType.Element -> 
                        match infoOfXamlElement info xaml with                                                             
                        |   Some (XamlElementInfo(name,typeName)) ->
                                match name with
                                |   None -> () // TODO: No name given
                                |   Some xamlId -> yield (xamlId, typeOfXamlElement typeName)
                                if not xaml.IsEmptyElement then yield! readElementChildren()
                        |   None -> ()
                    | XmlNodeType.EndElement -> more := false
                    | _ -> ()
        }
        
    let getXamlFileFromRootElement() =
        match  infoOfXamlElement info xaml with
        |   Some (XamlElementInfo(_name,typeName)) ->                 
                { Root = typeOfXamlElement typeName 
                  Position = posOfReader info xaml
                  Children = readElementChildren() |> Seq.toList }
        |   None -> XamlFile.FileStart info.FileName

    match xaml.Read() with
    | false -> XamlFile.FileStart info.FileName
    | true ->                
        if xaml.NodeType = XmlNodeType.XmlDeclaration then
            xaml.Read() |> ignore
        if xaml.NodeType = XmlNodeType.Element then
            getXamlFileFromRootElement()
        else failwithf "Expected Root element but got %A" xaml.NodeType

let createXmlReader(textReader:TextReader) =
    XmlReader.Create(textReader, XmlReaderSettings(IgnoreProcessingInstructions = true, IgnoreWhitespace = true))

let createTypeFromReader typeName (xamlInfo:XamlInfo) (reader: TextReader) =
    let xaml = 
        createXmlReader(reader) 
        |> readXamlFile (fileStart xamlInfo.FileName)

    ProvidedTypeDefinition(thisAssembly,rootNamespace,typeName,baseType=Some xaml.Root)
        |> addDefinitionLocation xaml.Position
        |+> (provideConstructor
                [] 
                (fun args -> 
                    match xamlInfo with 
                    | XamlInfo.Path path -> Expr.Coerce(<@@ XamlReader.Parse(File.ReadAllText(path)) @@>, xaml.Root)
                    | XamlInfo.Text text -> Expr.Coerce(<@@ XamlReader.Parse(text) @@>, xaml.Root))
            |> addXmlDoc (sprintf "Initializes a %s instance" typeName)
            |> addDefinitionLocation xaml.Position)
        |++> (xaml.Children
                |> List.map 
                    (fun (XamlId(pos, propertyName), resultType) ->
                        provideProperty
                            propertyName
                            resultType
                            (fun args -> Expr.Coerce(<@@ (%%args.[0]:Window).FindName(propertyName) @@>, resultType))
                        |> addDefinitionLocation pos))
        
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