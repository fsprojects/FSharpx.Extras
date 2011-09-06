namespace FSharpx

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

/// <summary>
/// Conversion functions from Action/Func to FSharpFunc
/// We need these because FuncConvert often makes C# type inference fail.
/// </summary>
[<Extension>]
type FSharpFunc =
    static member FromAction (f: Action) =
        fun () -> f.Invoke()
    
    static member FromAction (f: Action<_>) =
        fun x -> f.Invoke x

    static member FromAction (f: Action<_,_>) =
        fun x y -> f.Invoke(x,y)

    static member FromAction (f: Action<_,_,_>) =
        fun x y z -> f.Invoke(x,y,z)

    static member FromFunc (f: Func<_>) =
        fun () -> f.Invoke()

    static member FromFunc (f: Func<_,_>) =
        fun x -> f.Invoke x

    static member FromFunc (f: Func<_,_,_>) =
        fun x y -> f.Invoke(x,y)

    [<Extension>]
    static member Curry (f: Func<_,_,_>) =
        Func<_,Func<_,_>>(fun a -> Func<_,_>(fun b -> f.Invoke(a,b)))

[<Extension>]
type FSharpOption =
    [<Extension>]
    static member HasValue o = Option.isSome o

    [<Extension>]
    static member ToNullable o =
        match o with
        | Some x -> Nullable x
        | _ -> Nullable()

    [<Extension>]
    static member ToFSharpOption (n: Nullable<_>) =
        if n.HasValue
            then Some n.Value
            else None

    [<Extension>]
    static member ToFSharpOption v = 
        match box v with
        | null -> None
        | :? DBNull -> None
        | _ -> Some v

    [<Extension>]
    static member Some a = Option.Some a

    [<Extension>]
    static member Match (o, ifSome: Func<_,_>, ifNone: Func<_>) =
        match o with
        | Some x -> ifSome.Invoke x
        | _ -> ifNone.Invoke()

    [<Extension>]
    static member Match (o, ifSome: Func<_,_>, ifNone) =
        match o with
        | Some x -> ifSome.Invoke x
        | _ -> ifNone

    [<Extension>]
    static member Match (o, ifSome: Action<_>, ifNone: Action) =
        match o with
        | Some x -> ifSome.Invoke x
        | _ -> ifNone.Invoke()

    [<Extension>]
    static member Do (o, f: Action<_>) =
        match o with
        | Some v -> f.Invoke v
        | _ -> ()

    [<Extension>]
    static member OrElse (o, other) =
        match o with
        | Some x -> Some x
        | _ -> other

    [<Extension>]
    static member GetOrElse (o, other) =
        match o with
        | Some x -> x
        | _ -> other

    [<Extension>]
    static member GetOrElse (o, other: _ Func) =
        match o with
        | Some x -> x
        | _ -> other.Invoke()

    [<Extension>]
    static member ToFSharpChoice (o, other) =
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
      let mapper = Option.map2 (fun a b -> mapper.Invoke(a,b))
      let v = Option.bind f.Invoke o
      mapper o v

    [<Extension>]
    static member Aggregate (o, state, f: Func<_,_,_>) =
        Option.fold (fun s x -> f.Invoke(s,x)) state o

    [<Extension>]
    static member Where (o: _ option, pred: _ Predicate) =
        match o with
        | Some v -> if pred.Invoke v then Some v else None
        | _ -> None

    static member SomeUnit = Some()

    static member TryParseInt s =
        match Int32.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseInt (s, style, provider) =
        match Int32.TryParse(s, style, provider) with
        | true,v -> Some v
        | _ -> None

    static member TryParseDecimal s =
        match Decimal.TryParse s with
        | true,v -> Some v
        | _ -> None
    static member TryParseDecimal (s, style, provider) =
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
        
    static member TryCastInt (o: obj) = 
        match o with
        | :? int as i -> Some i
        | _ -> None

open Either
open Validation

[<Extension>]
type FSharpChoice =

    [<Extension>]
    static member Match (c, f1: Func<_,_>, f2: Func<_,_>) =
        match c with
        | Choice1Of2 x -> f1.Invoke x
        | Choice2Of2 y -> f2.Invoke y

    [<Extension>]
    static member Match (c, f1: Action<_>, f2: Action<_>) =
        match c with
        | Choice1Of2 x -> f1.Invoke x
        | Choice2Of2 y -> f2.Invoke y

    [<Extension>]
    static member Match (c, f1: Func<_,_>, f2: Func<_,_>, f3: Func<_,_>) =
        match c with
        | Choice1Of3 x -> f1.Invoke x
        | Choice2Of3 x -> f2.Invoke x
        | Choice3Of3 x -> f3.Invoke x

    [<Extension>]
    static member Match (c, f1: Action<_>, f2: Action<_>, f3: Action<_>) =
        match c with
        | Choice1Of3 x -> f1.Invoke x
        | Choice2Of3 x -> f2.Invoke x
        | Choice3Of3 x -> f3.Invoke x

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>) =
      Either.bind f.Invoke o

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>, mapper: Func<_,_,_>) =
      let mapper = Either.map2 (fun a b -> mapper.Invoke(a,b))
      let v = Either.bind f.Invoke o
      mapper o v

    [<Extension>]
    static member Select (o, f: Func<_,_>) = map f.Invoke o

    [<Extension>]
    static member Join (c: Choice<'a, string list>, inner: Choice<'b, string list>, outerKeySelector: Func<'a,'c>, innerKeySelector: Func<'b,'c>, resultSelector: Func<'a,'b,'d>) =
      Either.puree (fun a b -> resultSelector.Invoke(a,b)) 
      |> Validation.ap c 
      |> Validation.ap inner 

    [<Extension>]
    static member Ap (f: Choice<Func<_,_>, _>, x) =
      f
      |> Either.map (fun a -> a.Invoke)
      |> Either.ap x

    // validation

    static member Error (x: string) = Choice2Of2 [x]
    static member Errors x : Choice<_, string list> = Choice2Of2 x
    static member Ok x : Choice<_, string list> = Choice1Of2 x

    static member Validator (p: _ Predicate, errorMsg: string) =
        let v x = 
            if p.Invoke x
                then FSharpChoice.Ok x
                else FSharpChoice.Error errorMsg
        Func<_,_>(v)

    [<Extension>]
    static member ApV (f: Choice<Func<_,_>, _>, x) =
      f 
      |> Either.map (fun a -> a.Invoke)
      |> Validation.ap x

    [<Extension>]
    static member PureValidate x : Choice<_, string list> = Choice1Of2 x

    static member EnumerableValidator (f: Func<'a, Choice<'a, string list>>) : Func<'a seq, Choice<'a seq, string list>> =
      let ff = Validation.seqValidator f.Invoke >> Either.map (fun a -> a :> _ seq)
      Func<_,_>(ff)

    // constructors

    static member New1Of2<'a,'b> (a: 'a) : Choice<'a,'b> = Choice1Of2 a
    static member New2Of2<'a,'b> (b: 'b) : Choice<'a,'b> = Choice2Of2 b

    static member New1Of3<'a,'b,'c> (a: 'a) : Choice<'a,'b,'c> = Choice1Of3 a
    static member New2Of3<'a,'b,'c> (a: 'b) : Choice<'a,'b,'c> = Choice2Of3 a
    static member New3Of3<'a,'b,'c> (a: 'c) : Choice<'a,'b,'c> = Choice3Of3 a

[<Extension>]
type FSharpList =
    [<Extension>]
    static member Match (l, empty: Func<_>, nonempty: Func<_,_,_>) =
        match l with
        | [] -> empty.Invoke()
        | x::xs -> nonempty.Invoke(x,xs)

    [<Extension>]
    static member Choose (l, chooser: Func<_,_>) =
        List.choose chooser.Invoke l

    [<Extension>]
    static member Cons (l, e) = e::l

    static member New([<ParamArray>] values: 'a array) =
        Seq.toList values

    [<Extension>]
    static member ToFSharpList s = Seq.toList s

[<Extension>]
type FSharpSet =
    static member New([<ParamArray>] values: 'a array) =
        set values

    [<Extension>]
    static member ToFSharpSet values = set values

[<Extension>]
type FSharpMap =
    static member New([<ParamArray>] values) =
        Map.ofArray values

    [<Extension>]
    static member ToFSharpMap values = Map.ofSeq values

[<Extension>]
module Dictionary =
  let tryFind key (d: IDictionary<_,_>) =
    match d.TryGetValue key with
    | true,v -> Some v
    | _ -> None

  [<Extension>]
  let TryFind (d: IDictionary<_,_>, key) = tryFind key d
