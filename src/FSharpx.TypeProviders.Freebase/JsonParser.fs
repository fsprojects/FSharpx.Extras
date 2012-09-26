// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

/// A simple F#-portable parser for JSON data
namespace Utilities.Json 

[<RequireQualifiedAccessAttribute>]
type internal JsonValue =
    | String of string
    | NumDecimal of decimal
    | NumDouble of System.Double // Some values are too big to fit in System.Decimal
    | Obj of (string * JsonValue)[]
    | Array of JsonValue[]
    | Bool of bool
    | Null
    member this.TryGetValWithKey s =
        match this with
        | Obj kvps -> kvps |> Array.tryFind (fun (k,_) -> k=s) |> Option.map snd // assumes no duplicate keys
        | _ -> None
    member this.GetValWithKey s =
        match this with
        | Obj kvps -> 
            match kvps |> Array.tryPick (fun (k,v) -> if k=s then Some v else None) with 
            | Some res -> res
            | None -> failwith (sprintf "didn't find key '%s' in %A" s kvps)
        | _ -> failwith (sprintf "expected an object when looking for find key '%s' in JsonValue %A" s this)
    member this.GetStringValWithKey s = this.GetValWithKey s |> JsonValue.GetStringVal
    member this.GetOptionalStringValWithKey s dflt = defaultArg (this.TryGetValWithKey s |> Option.map JsonValue.GetStringVal) dflt
    static member GetStringVal             s = match s with String v -> v | Bool b -> (if b then "true" else "false") | Null -> null | _ -> failwith (sprintf "The JSON item had value '%+A' when a string was expected" s)
    static member GetIntVal                s = match s with NumDecimal v -> int v | Null -> 0                                        | _ -> failwith (sprintf "The JSON item had value '%+A' when an integer was expected" s)
    static member GetArrayVal f            s = match s with Array v -> Array.map f v | Null -> [| |]                                 | _ -> failwith (sprintf "The JSON item had value '%+A' when an array was expected" s)
    member this.GetArrayValWithKey         s = match this.GetValWithKey s with | Array v -> v | Null -> [| |]                        | v -> failwith (sprintf "key '%s' had value '%+A' when a string was expected" s v)
    member this.GetOptionalArrayValWithKey s = match this.TryGetValWithKey s with | None -> [| |] | Some (Array v) -> v | Some Null -> [| |] | Some v -> failwith (sprintf "key '%s' had value '%+A' when a string was expected" s v)

type internal Parser(jsonText:string) =
    let mutable i = 0
    let s = jsonText
    let skipWhitespace() =
        while i < s.Length && (s.[i]=' ' || s.[i]='\t' || s.[i]='\r' || s.[i]='\n') do
            i <- i + 1
    let isNumChar c = System.Char.IsDigit c || c='.' || c='e' || c='E' || c='+' || c='-'
    let throw() = raise <| new System.Exception(sprintf "Invalid Json starting at character %d, snippet = \n----\n%s\n-----\njson = \n------\n%s\n-------" i (jsonText.[(max 0 (i-10)).. (min (jsonText.Length-1) (i+10))]) jsonText)
    let ensure cond = if not cond then throw()

    let rec parseValue() =
        skipWhitespace()
        ensure(i < s.Length)
        match s.[i] with
        | '"' -> JsonValue.String(parseString())
        | '-' -> parseNum()
        | c when System.Char.IsDigit(c) -> parseNum()
        | '{' -> parseObject()
        | '[' -> parseArray()
        | 't' -> parseLiteral("true", JsonValue.Bool true)
        | 'f' -> parseLiteral("false", JsonValue.Bool false)
        | 'n' -> parseLiteral("null", JsonValue.Null)
        | _ -> throw()
    and parseString() =
        ensure(i < s.Length && s.[i] = '"')
        i <- i + 1
        let buf = new System.Text.StringBuilder()
        while i < s.Length && s.[i] <> '"' do
            if s.[i] = '\\' then
                ensure(i+1 < s.Length)
                match s.[i+1] with
                | 'b' -> buf.Append('\b') |> ignore
                | 'f' -> buf.Append('\f') |> ignore
                | 'n' -> buf.Append('\n') |> ignore
                | 't' -> buf.Append('\t') |> ignore
                | 'r' -> buf.Append('\r') |> ignore
                | '\\' -> buf.Append('\\') |> ignore
                | '/' -> buf.Append('/') |> ignore
                | '"' -> buf.Append('"') |> ignore
                | 'u' ->
                    ensure(i+5 < s.Length)
                    let hexdigit d = 
                        if d >= '0' && d <= '9' then int32 d - int32 '0'
                        elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
                        elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
                        else failwith "hexdigit" 
                    let unicodeGraphShort (s:string) =
                        if s.Length <> 4 then failwith "unicodegraph";
                        uint16 (hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3])
                    let makeUnicodeChar (c:int) =  [| byte(c % 256); byte(c / 256) |]
                    let bytes = makeUnicodeChar(int(unicodeGraphShort(s.Substring(i+2, 4))))
                    let chars = System.Text.UnicodeEncoding.Unicode.GetChars(bytes)
                    buf.Append(chars) |> ignore
                    i <- i + 4  // the \ and u will also be skipped past further below
                | _ -> throw()
                i <- i + 2  // skip past \ and next char
            else
                buf.Append(s.[i]) |> ignore
                i <- i + 1
        ensure(i < s.Length && s.[i] = '"')
        i <- i + 1
        buf.ToString()
    and parseNum() =
        let start = i
        while i < s.Length && isNumChar(s.[i]) do
            i <- i + 1
        let len = i - start
        match System.Decimal.TryParse(s.Substring(start,len), System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with  
        | true, x -> JsonValue.NumDecimal x
        | _ -> 
            match System.Double.TryParse(s.Substring(start,len), System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with  
            | true, x -> JsonValue.NumDouble x
            | _ -> throw()
    and parsePair() =
        let key = parseString()
        skipWhitespace()
        ensure(i < s.Length && s.[i] = ':')
        i <- i + 1
        skipWhitespace()
        key, parseValue()
    and parseObject() =
        ensure(i < s.Length && s.[i] = '{')
        i <- i + 1
        skipWhitespace()
        let pairs = ResizeArray<_>()
        if i<s.Length && s.[i]='"' then
            pairs.Add(parsePair())
            skipWhitespace()
            while i<s.Length && s.[i]=',' do
                i <- i + 1
                skipWhitespace()
                pairs.Add(parsePair())
                skipWhitespace()
        ensure(i < s.Length && s.[i] = '}')
        i <- i + 1
        JsonValue.Obj(pairs |> Seq.toArray)
    and parseArray() =
        ensure(i < s.Length && s.[i] = '[')
        i <- i + 1
        skipWhitespace()
        let vals = ResizeArray<_>()
        if i<s.Length && s.[i]<>']' then
            vals.Add(parseValue())
            skipWhitespace()
            while i<s.Length && s.[i]=',' do
                i <- i + 1
                skipWhitespace()
                vals.Add(parseValue())
                skipWhitespace()
        ensure(i < s.Length && s.[i] = ']')
        i <- i + 1
        JsonValue.Array(vals |> Seq.toArray)
    and parseLiteral(expected, r) =
        ensure(i+expected.Length < s.Length)
        for j in 0 .. expected.Length - 1 do
            ensure(s.[i+j] = expected.[j])
        i <- i + expected.Length
        r
    member __.Parse() = parseValue()

module internal JsonParser = 
    let parseJsonValue(jsonText : string) =
        let p = new Parser(jsonText)
        p.Parse()
