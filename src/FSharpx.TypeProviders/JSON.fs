module FSharpx.JSON

// Initial version of the parser from http://blog.efvincent.com/parsing-json-using-f/
// Simplyfied and fixed some minor bugs

open System
open System.Xml
open System.Xml.Linq
open System.Text
open Microsoft.FSharp.Reflection
open System.Collections.Generic
open FSharpx.Strings

type Token =
| OpenBracket | CloseBracket
| OpenArray | CloseArray
| Colon | Comma
| String of string
| Boolean of bool
| Null
| Number of string

let tokenize source=
    let rec parseString acc = function
        | '\\' :: '"' :: t -> // escaped quote
            parseString (acc + "\"") t
        | '"' :: t -> // closing quote terminates    
            acc, t
        | c :: t -> // otherwise accumulate
            parseString (acc + (c.ToString())) t
        | _ -> failwith "Malformed string."
 
    let rec token acc = function
        | (')' :: _) as t -> acc, t // closing paren terminates
        | ('}' :: _) as t -> acc, t // closing paren terminates
        | (']' :: _) as t -> acc, t // closing paren terminates
        | (':' :: _) as t -> acc, t // colon terminates
        | (',' :: _) as t -> acc, t // comma terminates
        | w :: t when Char.IsWhiteSpace(w) -> acc, t // whitespace terminates
        | [] -> acc, [] // end of list terminates
        | c :: t -> token (acc + (c.ToString())) t // otherwise accumulate chars

    let rec tokenize' acc = function
        | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t   // skip whitespace
        | '{' :: t -> tokenize' (OpenBracket :: acc) t
        | '}' :: t -> tokenize' (CloseBracket :: acc) t
        | '[' :: t -> tokenize' (OpenArray :: acc) t
        | ']' :: t -> tokenize' (CloseArray :: acc) t
        | ':' :: t -> tokenize' (Colon :: acc) t
        | ',' :: t -> tokenize' (Comma :: acc) t
        | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Token.Null :: acc) t
        | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
        | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false :: acc) t
        | '"' :: t -> // start of string
            let s, t' = parseString "" t
            tokenize' (Token.String(s) :: acc) t'        
        | '-' :: d :: t when Char.IsDigit(d) -> // start of negative number
            let n, t' = token ("-" + d.ToString()) t
            tokenize' (Token.Number(n) :: acc) t'
        | '+' :: d :: t 
        | d :: t when Char.IsDigit(d) || d = '.' -> // start of positive number
            let n, t' = token (d.ToString()) t
            tokenize' (Token.Number(n) :: acc) t'
        | [] -> List.rev acc // end of list terminates
        | _ -> failwith "Tokinzation error"

    tokenize' [] [for x in source -> x]

type Document = interface end

type internal Infrastucture =
    abstract member Serialize : StringBuilder -> StringBuilder
    abstract member ToXml: unit -> obj

type Text(text:string) =
    let mutable v = text

    let escape s = 
        s|> replace "\"" "\\\""

    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = sprintf "\"%s\"" (escape v)

    override this.Equals other =
        match other with
        | (:? Text as y) -> this.Value = y.Value
        | _ -> false

    interface Document
    interface Infrastucture with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"", escape v)
        member this.ToXml() = v :> obj

type Number(number:float) =
    let mutable v = number
    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = v.ToString()

    override this.Equals other =
        match other with
        | (:? Number as y) -> this.Value = y.Value
        | _ -> false

    interface Document
    interface Infrastucture with
        member this.Serialize sb = sb.Append(v.ToString(System.Globalization.CultureInfo.InvariantCulture))
        member this.ToXml() = v :> obj

type Boolean(boolean:bool) =
    let mutable v = boolean
    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = v.ToString()

    override this.Equals other =
        match other with
        | (:? Boolean as y) -> this.Value = y.Value
        | _ -> false

    interface Document
    interface Infrastucture with
        member this.Serialize sb = sb.Append(v.ToString().ToLower())
        member this.ToXml() = v :> obj

type JSONNull() =
    override this.ToString() = "null"

    override this.Equals other =
        match other with
        | :? JSONNull -> true
        | _ -> false

    interface Document
    interface Infrastucture with
        member this.Serialize sb = sb.Append "null"
        member this.ToXml() = null

type JArray(elements:List<Document>) =
    let mutable v = elements
    let serialize(sb:StringBuilder) =
        let isNotFirst = ref false
        sb.Append "[" |> ignore
        for element in v do
            if !isNotFirst then sb.Append "," |> ignore else isNotFirst := true
            (element :?> Infrastucture).Serialize(sb) |> ignore
        sb.Append "]"

    member this.Elements with get() = v
    override this.ToString() = (new StringBuilder() |> serialize).ToString()

    override this.Equals other =
        match other with
        | (:? JArray as y) ->
            if this.Elements.Count <> y.Elements.Count then false else
            this.Elements
              |> Seq.zip y.Elements
              |> Seq.forall (fun (a,b) -> a.Equals b)
        | _ -> false

    static member New() = new JArray(new List<Document>())

    interface Document
    interface Infrastucture with
        member this.Serialize sb = serialize sb
        member this.ToXml() = v |> Seq.map (fun item -> new XElement(XName.Get "item", (item:?> Infrastucture).ToXml())) :> obj

type JObject(properties:Dictionary<string,Document>) =
    let mutable v = properties
    let serialize(sb:StringBuilder) =
        let isNotFirst = ref false
        sb.Append "{"  |> ignore
        for property in v do
            if !isNotFirst then sb.Append "," |> ignore else isNotFirst := true
            sb.AppendFormat("\"{0}\":",property.Key)  |> ignore
            (property.Value:?> Infrastucture).Serialize(sb) |> ignore
        sb.Append "}"

    member this.Properties with get() = v
    
    override this.ToString() = (new StringBuilder() |> serialize).ToString()

    override this.Equals other =
        match other with
        | (:? JObject as y) ->
            if this.Properties.Count <> y.Properties.Count then false else
            this.Properties
              |> Seq.zip y.Properties
              |> Seq.forall (fun (a,b) -> a.Key = b.Key && a.Equals b)
        | _ -> false

    static member New() = new JObject(new Dictionary<string,Document>())

    interface Document
    interface Infrastucture with
        member this.Serialize sb = serialize sb
        member this.ToXml() =
            v 
            |> Seq.map (fun kv -> 
                    match kv.Value with
                    | (:? Text as v) -> new XAttribute(XName.Get kv.Key, v.Value) :> XObject
                    | (:? Boolean as v) -> new XAttribute(XName.Get kv.Key, v.Value) :> XObject
                    | (:? Number as v) -> new XAttribute(XName.Get kv.Key, v.Value) :> XObject
                    | _ -> new XElement(XName.Get kv.Key, (kv.Value :?> Infrastucture).ToXml()) :> XObject) :> obj

open System.Globalization

/// Parses a JSON source text and returns an JSON AST
let parse source =
    let map = function
    | Token.Number number -> 
        Number(Double.Parse(number, CultureInfo.InvariantCulture)) :> Document
    | Token.String text -> Text(text) :> Document
    | Token.Null -> JSONNull() :> Document
    | Token.Boolean(b) -> Boolean(b) :> Document
    | v -> failwith "Syntax Error, unrecognized token in map()"
 
    let rec parseValue = function
    | OpenBracket :: t -> parseJObject t
    | OpenArray :: t ->  parseArray t
    | h :: t -> map h, t
    | _ -> failwith "bad value"
 
    and parseArray = function
    | Comma :: t -> parseArray t
    | CloseArray :: t -> JArray.New() :> Document, t
    | t ->        
        let element, t' = parseValue t
        match parseArray t' with
        | (:? JArray as jArray),t'' -> 
            jArray.Elements.Insert(0,element)
            jArray :> Document,t''
        | _ -> failwith "Malformed JSON array"
    and parseJObject = function
    | Comma :: t -> parseJObject t
    | Token.String(name) :: Colon :: t -> 
        let value,t' = parseValue t
        match parseJObject t' with
        | (:? JObject as jObject),t'' ->
            jObject.Properties.[name] <- value
            jObject :> Document,t''
        | _ -> failwith "Malformed JSON object" 
    | CloseBracket :: t -> JObject.New() :> Document, t
    | _ -> failwith "Malformed JSON object"
    
    tokenize source 
    |> parseValue
    |> fst

let fromXml(xml:XDocument) =
    let rec createJArray (elements:XElement seq) =
        let jArray = JArray.New()
        for element in elements do
            createJObject element 
            |> jArray.Elements.Add
        jArray
    and createJObject (element:XElement) =
        let jObject = JObject.New()
        for attribute in element.Attributes() do
            jObject.Properties.[attribute.Name.LocalName] <- Text (attribute.Value)

        element.Elements()
        |> Seq.groupBy (fun x -> x.Name.LocalName)
        |> Seq.iter (fun (key,childs) ->
                match Seq.toList childs with
                | child::[] -> jObject.Properties.[singularize key] <- createJObject child
                | childs -> jObject.Properties.[pluralize key] <- createJArray childs)
        jObject

    createJObject xml.Root :> Document
    
[<AutoOpen>]
module Extension =
    type Document with 
        member this.GetProperty propertyName = (this :?> JObject).Properties.[propertyName]
        member this.HasProperty propertyName = (this :?> JObject).Properties.ContainsKey propertyName
        member this.GetText propertyName = (this.GetProperty(propertyName) :?> Text).Value
        member this.GetNumber propertyName = (this.GetProperty(propertyName) :?> Number).Value
        member this.GetBoolean propertyName = (this.GetProperty(propertyName) :?> Boolean).Value
        member this.GetJObject propertyName = this.GetProperty(propertyName) :?> JObject
        member this.GetJArray propertyName = 
            let this = (this :?> JObject)
            match this.Properties.TryGetValue propertyName with
            | true,jArray -> jArray :?> JArray 
            | _ -> 
                let jArray = JArray.New()
                this.Properties.[propertyName] <- jArray
                jArray

        member this.AddProperty(propertyName,document) = (this :?> JObject).Properties.[propertyName] <- document; this
        member this.AddTextProperty(propertyName,text) = (this :?> JObject).Properties.[propertyName] <- Text(text); this
        member this.AddBoolProperty(propertyName,boolean) = (this :?> JObject).Properties.[propertyName] <- Boolean(boolean); this
        member this.AddNumberProperty(propertyName,number) = (this :?> JObject).Properties.[propertyName] <- Number(number); this
        member this.AddElement element = (this :?> JArray).Elements.Add element; this
        
        member this.RemoveProperty propertyName = (this :?> JObject).Properties.Remove propertyName |> ignore

        member this.ToXml() = (this :?> Infrastucture).ToXml() :?> XObject seq

    type System.Xml.Linq.XDocument with
        member this.ToJson() = fromXml this