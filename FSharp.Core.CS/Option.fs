namespace Microsoft.FSharp.Core

open System
open System.Runtime.CompilerServices

[<AutoOpen>]
module internal Op =
    let (==) a b = LanguagePrimitives.PhysicalEquality a b

[<Extension>]
type Opt =
    [<Extension>]
    static member HasValue o = Option.isSome o

    [<Extension>]
    static member ToNullable o =
        match o with
        | Some x -> Nullable x
        | _ -> Nullable()

    [<Extension>]
    static member ToOption (n: Nullable<_>) =
        if n.HasValue
            then Some n.Value
            else None

    [<Extension>]
    static member ToOption v = if v == null then None else Some v

    [<Extension>]
    static member Some a = Option.Some a

    [<Extension>]
    static member Match (o, ifSome: Func<_,_>, ifNone: Func<_>) =
        match o with
        | Some x -> ifSome.Invoke x
        | _ -> ifNone.Invoke()

    [<Extension>]
    static member Match (o, ifSome: Action<_>, ifNone: Action) =
        match o with
        | Some x -> ifSome.Invoke x
        | _ -> ifNone.Invoke()

    [<Extension>]
    static member ToChoice (o, other) =
        match o with
        | Some v -> Choice1Of2 v
        | _ -> Choice2Of2 other

    // LINQ
    [<Extension>]
    static member Select (o, f: Func<_,_>) = Option.map f.Invoke o

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>) = Option.bind f.Invoke o

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>, mapper: Func<_,_,_>) =
        let c = Option.bind f.Invoke o
        match o,c with
        | Some a,Some b -> mapper.Invoke(a,b) |> Some
        | _ -> None

    [<Extension>]
    static member Aggregate (o, state, f: Func<_,_,_>) =
        Option.fold (fun s x -> f.Invoke(s,x)) state o

    static member SomeUnit = Some()

    static member TryParseInt s =
        match Int32.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseInt (s, style, provider) =
        match Int32.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None

    static member TryParseDec s =
        match Decimal.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseDec (s, style, provider) =
        match Decimal.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None

    static member TryParseDouble s =
        match Double.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseDouble (s, style, provider) =
        match Double.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None
        
    static member TryParseFloat s =
        match Single.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseFloat (s, style, provider) =
        match Single.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None
        
    static member TryParseInt16 s =
        match Int16.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseInt16 (s, style, provider) =
        match Int16.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None

    static member TryParseInt64 s =
        match Int64.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseInt64 (s, style, provider) =
        match Int64.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None
        
    static member TryParseByte s =
        match Byte.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseByte (s, style, provider) =
        match Byte.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None
        
    static member TryParseDateTime s =
        match DateTime.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseDateTime (s, style, provider) =
        match DateTime.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None
        
    static member TryParseDateTimeOffset s =
        match DateTimeOffset.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseDateTimeOffset (s, style, provider) =
        match DateTimeOffset.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None
        