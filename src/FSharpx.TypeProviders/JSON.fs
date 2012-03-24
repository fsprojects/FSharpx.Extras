module FSharpx.TypeProviders.JSONParser

// Initial version of the parser from http://blog.efvincent.com/parsing-json-using-f/
// Simplyfied, added AST and fixed some minor bugs

open System
open System.Xml
open System.Xml.Linq
open System.Text
open Microsoft.FSharp.Reflection

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
| Text of string ref
| Number of float ref
| Boolean of bool ref
| Null
| JArray of JSON list ref
| JObject of Map<string,JSON> ref
    member json.HasProperty propertyName = 
        match json with
        | JObject map -> Map.containsKey propertyName !map

    member json.RemoveProperty propertyName = 
        match json with
        | JObject map -> map := Map.remove propertyName !map

    member json.GetProperty propertyName = 
        match json with
        | JObject map -> Map.find propertyName !map

    member json.GetText() = 
        match json with
        | Text t -> !t

    member json.GetNumber() = 
        match json with
        | Number n -> !n

    member json.GetBoolean() = 
        match json with
        | Boolean b -> !b

    member json.GetSubElements() = 
        match json with
        | JArray childs -> !childs

    member json.SetText(propertyName,text) = 
        match json with
        | JObject map -> map := Map.add propertyName (Text (ref text)) !map

    member json.SetNumber(propertyName,number) = 
        match json with
        | JObject map -> map := Map.add propertyName (Number (ref number)) !map

    member json.SetBoolean(propertyName,boolean) = 
        match json with
        | JObject map -> map := Map.add propertyName (Boolean (ref boolean)) !map

    member json.AddSubElement(propertyName,child) =
        match json with
        | JObject map ->
          match Map.tryFind propertyName !map with
          | Some (JArray childs) -> map := Map.add propertyName (JArray (ref (child :: !childs))) !map
          | _ -> map := Map.add propertyName (JArray (ref [])) !map

    member json.ToXml() =
        let rec toXml' json =
            match json with
            | Text t -> !t :> obj
            | Number n -> !n :> obj
            | Boolean b -> !b :> obj
            | Null -> null
            | JArray a -> !a |> Seq.map (fun item -> new XElement(XName.Get "item", toXml' item)) :> obj
            | JObject map -> !map |> Map.toSeq |> Seq.map (fun (k,v) -> new XElement(XName.Get k, toXml' v)) :> obj 
        toXml' json :?> XElement seq

    override json.ToString() =
        let sb = new StringBuilder()
        let rec writeToStringBuilder = function
        | Text t -> sb.AppendFormat("\"{0}\"",!t)  |> ignore
        | Number n -> sb.Append !n |> ignore
        | Boolean b -> 
            match !b with
            | true -> sb.Append "true" |> ignore
            | false -> sb.Append "false" |> ignore
        | Null -> sb.Append "null" |> ignore
        | JArray a -> 
            let isNotFirst = ref false
            sb.Append "[" |> ignore
            !a
              |> List.iter 
                    (fun element -> 
                        if !isNotFirst then sb.Append "," |> ignore else isNotFirst := true
                        writeToStringBuilder element |> ignore)
            sb.Append "]"  |> ignore
        | JObject map -> 
            let isNotFirst = ref false
            sb.Append "{"  |> ignore
            !map
              |> Map.iter 
                    (fun key value -> 
                        if !isNotFirst then sb.Append "," |> ignore else isNotFirst := true
                        sb.AppendFormat("\"{0}\":",key)  |> ignore
                        writeToStringBuilder value |> ignore)
            sb.Append "}"  |> ignore

        writeToStringBuilder json
        sb.ToString()

let emptyJObject = JObject (ref Map.empty)
let addProperty key value = function
| JObject properties -> JObject(ref (Map.add key value !properties))
| _ -> failwith "Malformed JSON object" 

open System.Globalization

/// Parses a JSON source text and returns an JSON AST
let parse source =
    let map = function
    | Token.Number number -> 
        Number (ref (Double.Parse(number, CultureInfo.InvariantCulture)))
    | Token.String text -> Text (ref text)
    | Token.Null -> JSON.Null
    | Token.Boolean(b) -> Boolean (ref b)
    | v -> failwith "Syntax Error, unrecognized token in map()"
 
    let rec parseValue = function
    | OpenBracket :: t -> parseJObject t
    | OpenArray :: t ->  parseArray t
    | h :: t -> map h, t
    | _ -> failwith "bad value"
 
    and parseArray = function
    | Comma :: t -> parseArray t
    | CloseArray :: t -> JArray(ref []), t
    | t ->        
        let element, t' = parseValue t
        match parseArray t' with
        | JArray(elements),t'' -> JArray (ref (element :: !elements)),t''
        | _ -> failwith "Malformed JSON array"
    and parseJObject = function
    | Comma :: t -> parseJObject t
    | Token.String(name) :: Colon :: t -> 
        let value,t' = parseValue t
        let jObject,t'' = parseJObject t'
        addProperty name value jObject,t''
    | CloseBracket :: t -> emptyJObject, t
    | _ -> failwith "Malformed JSON object"
    
    tokenize source 
    |> parseValue
    |> fst


let rec toJSON(value:obj) = 
    match value with
    | x when x = null-> Null
    | :? string as s -> Text (ref s)
    | :? int as n -> Number (ref (float n))
    | :? float as n -> Number (ref n)
    | :? bool as b -> Boolean (ref b)