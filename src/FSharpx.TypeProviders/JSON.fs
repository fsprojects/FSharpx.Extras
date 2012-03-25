module FSharpx.TypeProviders.JSONParser

// Initial version of the parser from http://blog.efvincent.com/parsing-json-using-f/
// Simplyfied, added AST and fixed some minor bugs

open System
open System.Xml
open System.Xml.Linq
open System.Text
open Microsoft.FSharp.Reflection
open System.Collections.Generic

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

type JSON =
    abstract member Serialize : StringBuilder -> StringBuilder
    abstract member ToXml: unit -> obj

type Text(text:string) =
    let mutable v = text

    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = sprintf "\"%s\"" v

    interface JSON with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"",v)
        member this.ToXml() = v :> obj

type Number(number:float) =
    let mutable v = number
    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = v.ToString()

    interface JSON with
        member this.Serialize sb = sb.Append(v)
        member this.ToXml() = v :> obj

type Boolean(boolean:bool) =
    let mutable v = boolean
    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = v.ToString()

    interface JSON with
        member this.Serialize sb = sb.Append(v.ToString().ToLower())
        member this.ToXml() = v :> obj

type JSONNull() =
    override this.ToString() = "null"
    interface JSON with
        member this.Serialize sb = sb.Append "null"
        member this.ToXml() = null

type JArray(elements:List<JSON>) =
    let mutable v = elements
    let serialize(sb:StringBuilder) =
        let isNotFirst = ref false
        sb.Append "[" |> ignore
        for element in v do
            if !isNotFirst then sb.Append "," |> ignore else isNotFirst := true
            element.Serialize(sb) |> ignore
        sb.Append "]"

    member this.Elements with get() = v
    override this.ToString() = (new StringBuilder() |> serialize).ToString()

    static member New() = new JArray(new List<JSON>())

    interface JSON with
        member this.Serialize sb = serialize sb
        member this.ToXml() = v |> Seq.map (fun item -> new XElement(XName.Get "item", item.ToXml())) :> obj

type JObject(properties:Dictionary<string,JSON>) =
    let mutable v = properties
    let serialize(sb:StringBuilder) =
        let isNotFirst = ref false
        sb.Append "{"  |> ignore
        for property in v do
            if !isNotFirst then sb.Append "," |> ignore else isNotFirst := true
            sb.AppendFormat("\"{0}\":",property.Key)  |> ignore
            property.Value.Serialize(sb) |> ignore
        sb.Append "}"

    member this.Properties with get() = v
    member this.AddTextProperty(propertyName,text) = v.[propertyName] <- Text(text); this
    member this.AddBoolProperty(propertyName,boolean) = v.[propertyName] <- Boolean(boolean); this
    member this.AddNumberProperty(propertyName,number) = v.[propertyName] <- Number(number); this

    override this.ToString() = (new StringBuilder() |> serialize).ToString()

    static member New() = new JObject(new Dictionary<string,JSON>())

    interface JSON with
        member this.Serialize sb = serialize sb
        member this.ToXml() = v |> Seq.map (fun kv -> new XElement(XName.Get kv.Key, kv.Value.ToXml())) :> obj


open System.Globalization

/// Parses a JSON source text and returns an JSON AST
let parse source =
    let map = function
    | Token.Number number -> 
        Number(Double.Parse(number, CultureInfo.InvariantCulture)) :> JSON
    | Token.String text -> Text(text) :> JSON
    | Token.Null -> JSONNull() :> JSON
    | Token.Boolean(b) -> Boolean(b) :> JSON
    | v -> failwith "Syntax Error, unrecognized token in map()"
 
    let rec parseValue = function
    | OpenBracket :: t -> parseJObject t
    | OpenArray :: t ->  parseArray t
    | h :: t -> map h, t
    | _ -> failwith "bad value"
 
    and parseArray = function
    | Comma :: t -> parseArray t
    | CloseArray :: t -> JArray.New() :> JSON, t
    | t ->        
        let element, t' = parseValue t
        match parseArray t' with
        | (:? JArray as jArray),t'' -> 
            jArray.Elements.Insert(0,element)
            jArray :> JSON,t''
        | _ -> failwith "Malformed JSON array"
    and parseJObject = function
    | Comma :: t -> parseJObject t
    | Token.String(name) :: Colon :: t -> 
        let value,t' = parseValue t
        match parseJObject t' with
        | (:? JObject as jObject),t'' ->
            jObject.Properties.[name] <- value
            jObject :> JSON,t''
        | _ -> failwith "Malformed JSON object" 
    | CloseBracket :: t -> JObject.New() :> JSON, t
    | _ -> failwith "Malformed JSON object"
    
    tokenize source 
    |> parseValue
    |> fst

let rec toJSON(value:obj) = 
    match value with
    | x when x = null-> JSONNull() :> JSON
    | :? string as s -> Text(s) :> JSON
    | :? int as n -> Number(float n) :> JSON
    | :? float as n -> Number(n) :> JSON
    | :? bool as b -> Boolean(b) :> JSON