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

[<RequireQualifiedAccess>]
type Token =
    | OpenBracket | CloseBracket
    | OpenArray | CloseArray
    | Colon | Comma
    | String of string
    | Date of DateTime
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
        | '{' :: t -> tokenize' (Token.OpenBracket :: acc) t
        | '}' :: t -> tokenize' (Token.CloseBracket :: acc) t
        | '[' :: t -> tokenize' (Token.OpenArray :: acc) t
        | ']' :: t -> tokenize' (Token.CloseArray :: acc) t
        | ':' :: t -> tokenize' (Token.Colon :: acc) t
        | ',' :: t -> tokenize' (Token.Comma :: acc) t
        | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Token.Null :: acc) t
        | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Token.Boolean true :: acc) t
        | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Token.Boolean false :: acc) t
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

type IDocument = interface end

type internal Infrastucture =
    abstract member Serialize : StringBuilder -> StringBuilder
    abstract member ToXml: unit -> obj

type Text(text:string) =
    let mutable v = text

    let escape s = 
        s|> replace "\"" "\\\""

    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = sprintf "\"%s\"" (escape v)

    override this.GetHashCode() = hash this.Value

    override this.Equals other =
        match other with
        | (:? Text as y) -> this.Value = y.Value
        | _ -> false

    interface IDocument
    interface Infrastucture with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"", escape v)
        member this.ToXml() = v :> obj

type Date(date:DateTime) =
    let mutable v = date

    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = sprintf "\"%s\"" (v.ToString())

    override this.GetHashCode() = hash this.Value

    override this.Equals other =
        match other with
        | (:? Date as y) -> this.Value = y.Value
        | _ -> false

    interface IDocument
    interface Infrastucture with
        member this.Serialize sb = sb.Append(v.ToString(System.Globalization.CultureInfo.InvariantCulture))
        member this.ToXml() = v :> obj


type Number(number:float) =
    let mutable v = number
    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = v.ToString()

    override this.GetHashCode() = hash this.Value

    override this.Equals other =
        match other with
        | (:? Number as y) -> this.Value = y.Value
        | _ -> false

    interface IDocument
    interface Infrastucture with
        member this.Serialize sb = sb.Append(v.ToString(System.Globalization.CultureInfo.InvariantCulture))
        member this.ToXml() = v :> obj

type Boolean(boolean:bool) =
    let mutable v = boolean
    member this.Value with get() = v and set (value) = v <- value
    override this.ToString() = v.ToString()

    override this.GetHashCode() = hash this.Value

    override this.Equals other =
        match other with
        | (:? Boolean as y) -> this.Value = y.Value
        | _ -> false

    interface IDocument
    interface Infrastucture with
        member this.Serialize sb = sb.Append(v.ToString().ToLower())
        member this.ToXml() = v :> obj

type JSONNull() =
    override this.ToString() = "null"

    override this.GetHashCode() = 1

    override this.Equals other =
        match other with
        | :? JSONNull -> true
        | _ -> false

    interface IDocument
    interface Infrastucture with
        member this.Serialize sb = sb.Append "null"
        member this.ToXml() = null

type JArray(elements:List<IDocument>) =
    let mutable v = elements
    let serialize(sb:StringBuilder) =
        let isNotFirst = ref false
        sb.Append "[" |> ignore
        for element in v do
            if !isNotFirst then sb.Append "," |> ignore else isNotFirst := true
            (element :?> Infrastucture).Serialize(sb) |> ignore
        sb.Append "]"

    member this.Elements = v

    override this.ToString() = (new StringBuilder() |> serialize).ToString()

    override this.GetHashCode() = 
        let mutable hash = 1
        for w in this.Elements do hash <- 31 * hash + Unchecked.hash w
        hash

    override this.Equals other =
        match other with
        | (:? JArray as y) ->
            if this.Elements.Count <> y.Elements.Count then false else
            this.Elements
              |> Seq.zip y.Elements
              |> Seq.forall (fun (a,b) -> a.Equals b)
        | _ -> false

    static member New() = new JArray(new List<IDocument>())

    interface IDocument
    interface Infrastucture with
        member this.Serialize sb = serialize sb
        member this.ToXml() = v |> Seq.map (fun item -> new XElement(XName.Get "item", (item:?> Infrastucture).ToXml())) :> obj

type JObject(properties:Dictionary<string,IDocument>) =
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

    override x.GetHashCode() = 
        let mutable hash = 1
        for w in v do hash <- 31 * hash + Unchecked.hash w
        hash

    override this.Equals other =
        match other with
        | (:? JObject as y) ->
            if this.Properties.Count <> y.Properties.Count then false else
            this.Properties
              |> Seq.zip y.Properties
              |> Seq.forall (fun (a,b) -> a.Key = b.Key && a.Equals b)
        | _ -> false

    static member New() = new JObject(new Dictionary<string,IDocument>())

    interface IDocument
    interface Infrastucture with
        member this.Serialize sb = serialize sb
        member this.ToXml() =
            v 
            |> Seq.map (fun kv -> 
                    match kv.Value with
                    | (:? Text as v) -> new XAttribute(XName.Get kv.Key, v.Value) :> XObject
                    | (:? Boolean as v) -> new XAttribute(XName.Get kv.Key, v.Value) :> XObject
                    | (:? Number as v) -> new XAttribute(XName.Get kv.Key, v.Value) :> XObject
                    | (:? Date as v) -> new XAttribute(XName.Get kv.Key, v.Value) :> XObject
                    | _ -> new XElement(XName.Get kv.Key, (kv.Value :?> Infrastucture).ToXml()) :> XObject) :> obj

open System.Globalization

/// Parses a JSON source text and returns an JSON AST
let parse source =

    let (|Date|String|) input = 
        let msDateRegex = new System.Text.RegularExpressions.Regex(@"^\\\/Date\((-?\d+)(?:-\d+)?\)\\\/$")
        let iso8601Regex = new System.Text.RegularExpressions.Regex(@"^([\+-]?\d{4}(?!\d{2}\b))((-?)((0[1-9]|1[0-2])(\3([12]\d|0[1-9]|3[01]))?|W([0-4]\d|5[0-2])(-?[1-7])?|(00[1-9]|0[1-9]\d|[12]\d{2}|3([0-5]\d|6[1-6])))([T\s]((([01]\d|2[0-3])((:?)[0-5]\d)?|24\:?00)([\.,]\d+(?!:))?)?(\17[0-5]\d([\.,]\d+)?)?((?<IsUTC>[zZ])|([\+-])([01]\d|2[0-3]):?([0-5]\d)?)?)?)?$")
        let matchesMS = msDateRegex.Match(input)
        if matchesMS.Success then
            matchesMS.Groups.[1].Value 
            |> Double.Parse 
            |> (new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)).AddMilliseconds 
            |> (fun x -> Date(x))
        else
            let dateTimeStylesForUtc = function
                | true -> DateTimeStyles.AssumeUniversal ||| DateTimeStyles.AdjustToUniversal
                | false -> DateTimeStyles.AssumeLocal ||| DateTimeStyles.AllowWhiteSpaces
                
            let matches = iso8601Regex.Match(input)
            if matches.Success then
                input 
                |> fun s -> DateTime.TryParse(s, CultureInfo.InvariantCulture, dateTimeStylesForUtc matches.Groups.["IsUTC"].Success)
                |> (fun (parsed, d) -> if parsed then 
                                         Date(d)
                                       else
                                         String(input))
            else
                String(input)

    let map = function
    | Token.Number number -> 
        Number(Double.Parse(number, CultureInfo.InvariantCulture)) :> IDocument
    | Token.String text -> 
        match text with
        | Date(d) -> Date(d) :> IDocument
        | String(st) -> Text(st) :> IDocument
    | Token.Null -> JSONNull() :> IDocument
    | Token.Boolean(b) -> Boolean(b) :> IDocument
    | v -> failwith "Syntax Error, unrecognized token in map()"
 
    let rec parseValue = function
    | Token.OpenBracket :: t -> parseJObject t
    | Token.OpenArray :: t ->  parseArray t
    | h :: t -> map h, t
    | _ -> failwith "bad value"
 
    and parseArray = function
    | Token.Comma :: t -> parseArray t
    | Token.CloseArray :: t -> JArray.New() :> IDocument, t
    | t ->        
        let element, t' = parseValue t
        match parseArray t' with
        | (:? JArray as jArray),t'' -> 
            jArray.Elements.Insert(0,element)
            jArray :> IDocument,t''
        | _ -> failwith "Malformed JSON array"
    and parseJObject = function
    | Token.Comma :: t -> parseJObject t
    | Token.String(name) :: Token.Colon :: t -> 
        let value,t' = parseValue t
        match parseJObject t' with
        | (:? JObject as jObject),t'' ->
            jObject.Properties.[name] <- value
            jObject :> IDocument,t''
        | _ -> failwith "Malformed JSON object" 
    | Token.CloseBracket :: t -> JObject.New() :> IDocument, t
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

    createJObject xml.Root :> IDocument
    
module DocumentExtensions =
    type IDocument with 
        member this.GetProperty propertyName = (this :?> JObject).Properties.[propertyName]
        member this.HasProperty propertyName = (this :?> JObject).Properties.ContainsKey propertyName
        member this.GetText propertyName = (this.GetProperty(propertyName) :?> Text).Value
        member this.GetDate propertyName = (this.GetProperty(propertyName) :?> Date).Value
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
        member this.AddDateProperty(propertyName,date) = (this :?> JObject).Properties.[propertyName] <- Date(date); this
        member this.AddElement element = (this :?> JArray).Elements.Add element; this
        
        member this.RemoveProperty propertyName = (this :?> JObject).Properties.Remove propertyName |> ignore

        member this.ToXml() = (this :?> Infrastucture).ToXml() :?> XObject seq

    type System.Xml.Linq.XDocument with
        member this.ToJson() = fromXml this