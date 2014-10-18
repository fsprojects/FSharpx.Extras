namespace FSharpx.Functional

open System
open System.Globalization

[<AutoOpen>]
module Prelude =
    let inline flip f a b = f b a
    let inline curry f a b = f(a,b)
    let inline uncurry f (a,b) = f a b
    let inline swap (a,b) = (b,a)
    let inline konst a _ = a
    let inline konst2 a _ _ = a
  
    /// Creates a pair
    let inline tuple2 a b = a,b
    /// Creates a 3-tuple
    let inline tuple3 a b c = a,b,c
    /// Creates a 4-tuple
    let inline tuple4 a b c d = a,b,c,d
    /// Creates a 5-tuple
    let inline tuple5 a b c d e = a,b,c,d,e
    /// Creates a 6-tuple
    let inline tuple6 a b c d e f = a,b,c,d,e,f

    let rec fix f x = f (fix f) x
    let rec fix2 f x y = f (fix2 f) x y
    let rec fix3 f x y z = f (fix3 f) x y z

    type Boolean with
        static member parse x =
            match bool.TryParse(x) with
            | true,v -> Some v
            | _ -> None
            
    type Int16 with
        static member parseWithOptions style provider x =
            match Int16.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Int16.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int32 with
        static member parseWithOptions style provider x =
            match Int32.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Int32.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Decimal with
        static member parseWithOptions style provider x =
            match Decimal.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Decimal.parseWithOptions NumberStyles.Currency CultureInfo.InvariantCulture x

    type Byte with
        static member parseWithOptions style provider x =
            match Byte.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Byte.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Int64 with
        static member parseWithOptions style provider x =
            match Int64.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Int64.parseWithOptions NumberStyles.Integer CultureInfo.InvariantCulture x

    type Single with
        static member parseWithOptions style provider x =
            match Single.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Single.parseWithOptions NumberStyles.Float CultureInfo.InvariantCulture x

    type Double with
        static member parseWithOptions style provider x =
            match Double.TryParse(x, style, provider) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            Double.parseWithOptions NumberStyles.Float CultureInfo.InvariantCulture x

    type DateTime with
        static member parseWithOptions style provider x =
            match DateTime.TryParse(x, provider, style) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            DateTime.parseWithOptions DateTimeStyles.None CultureInfo.InvariantCulture x

        static member parseExactWithOptions style provider (formats: string[]) x =
            match DateTime.TryParseExact(x, formats, provider, style) with
            | true,v -> Some v
            | _ -> None

        static member parseExact formats x =
            DateTime.parseExactWithOptions DateTimeStyles.None CultureInfo.InvariantCulture formats x

    type DateTimeOffset with
        static member parseWithOptions style provider x =
            match DateTimeOffset.TryParse(x, provider, style) with
            | true,v -> Some v
            | _ -> None
            
        static member parse x = 
            DateTimeOffset.parseWithOptions DateTimeStyles.None CultureInfo.InvariantCulture x

        static member parseExactWithOptions style provider (formats: string[]) x =
            match DateTimeOffset.TryParseExact(x, formats, provider, style) with
            | true,v -> Some v
            | _ -> None

        static member parseExact formats x =
            DateTimeOffset.parseExactWithOptions DateTimeStyles.None CultureInfo.InvariantCulture formats x

    // Active patterns
    let (|Boolean|_|) = Boolean.parse
    let (|Int32|_|) = Int32.parse
    let (|Double|_|) = Double.parse
