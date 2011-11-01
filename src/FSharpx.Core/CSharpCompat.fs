namespace FSharpx

open System
open System.Net
open System.Collections.Generic
open System.Runtime.CompilerServices
open Microsoft.FSharp.Control.WebExtensions

type L =
    static member F (f: Func<_>) = f
    static member F (f: Func<_,_>) = f
    static member F (f: Func<_,_,_>) = f

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
type Funcs =
    [<Extension>]
    static member ToFunc (a: Action<_>) =
        Func<_,_>(a.Invoke)
  
    [<Extension>]
    static member ToFunc (a: Action<_,_>) =
        Func<_,_,_>(curry a.Invoke)
  
    [<Extension>]
    static member ToFunc (f: Action<_,_,_>) =
        Func<_,_,_,_>(fun a b c -> f.Invoke(a,b,c))

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
      let mapper = Option.lift2 (curry mapper.Invoke)
      let v = Option.bind f.Invoke o
      mapper o v

    [<Extension>]
    static member Aggregate (o, state, f: Func<_,_,_>) =
        Option.fold (curry f.Invoke) state o

    [<Extension>]
    static member Where (o: _ option, pred: _ Predicate) =
      Option.filter pred.Invoke o

    static member SomeUnit = Some()

    static member ParseInt s = Int32.parse s
    static member ParseInt (s, style, provider) = Int32.parseWithOptions style provider s

    static member ParseDecimal s = Decimal.parse s
    static member ParseDecimal (s, style, provider) = Decimal.parseWithOptions style provider s

    static member ParseDouble s = Double.parse s
    static member ParseDouble (s, style, provider) = Double.parseWithOptions style provider s

    static member ParseFloat s = Single.parse s
    static member ParseFloat (s, style, provider) = Single.parseWithOptions style provider s

    static member ParseInt16 s = Int16.parse s
    static member ParseInt16 (s, style, provider) = Int16.parseWithOptions style provider s

    static member ParseInt64 s = Int64.parse s
    static member ParseInt64 (s, style, provider) = Int64.parseWithOptions style provider s

    static member ParseByte s = Byte.parse s
    static member ParseByte (s, style, provider) = Byte.parseWithOptions style provider s

    static member ParseDateTime s = DateTime.parse s
    static member ParseDateTime (s, style, provider) = DateTime.parseWithOptions style provider s

    static member ParseDateTimeOffset s = DateTimeOffset.parse s
    static member ParseDateTimeOffset (s, style, provider) = DateTimeOffset.parseWithOptions style provider s

[<Extension>]
type FSharpChoice =

    static member Cast (o: obj) = Choice.cast o

    [<Extension>]
    static member ToFSharpOption c = Option.fromChoice c

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
        Choice.bind f.Invoke o

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>, mapper: Func<_,_,_>) =
        let mapper = Choice.lift2 (curry mapper.Invoke)
        let v = Choice.bind f.Invoke o
        mapper o v

    [<Extension>]
    static member Select (o, f: Func<_,_>) = Choice.map f.Invoke o

    [<Extension>]
    static member Join (c: Choice<'a, string list>, inner: Choice<'b, string list>, outerKeySelector: Func<'a,'c>, innerKeySelector: Func<'b,'c>, resultSelector: Func<'a,'b,'d>) =
        Choice.returnM (curry resultSelector.Invoke) 
        |> Validation.ap c 
        |> Validation.ap inner 

    [<Extension>]
    static member Ap (f: Choice<Func<_,_>, _>, x) =
        f
        |> Choice.map (fun a -> a.Invoke)
        |> Choice.ap x

    [<Extension>]
    static member SelectSecond (o, f: Func<_,_>) = Choice.mapSecond f.Invoke o

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
        |> Choice.map (fun a -> a.Invoke)
        |> Validation.ap x

    [<Extension>]
    static member PureValidate x : Choice<_, string list> = Choice1Of2 x

    static member EnumerableValidator (f: Func<'a, Choice<'a, string list>>) : Func<'a seq, Choice<'a seq, string list>> =
        let ff = Validation.seqValidator f.Invoke >> Choice.map (fun a -> a :> _ seq)
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
    static member TryFind (l, pred: _ Predicate) = 
        List.tryFind pred.Invoke l

    [<Extension>]
    static member TryFind (l, value) = 
        List.tryFind ((=) value) l

    [<Extension>]
    static member Cons (l, e) = e::l

    static member Create([<ParamArray>] values: 'a array) =
        Seq.toList values

    [<Extension>]
    static member ToFSharpList s = Seq.toList s

[<Extension>]
type FSharpSet =
    static member Create([<ParamArray>] values: 'a array) =
        set values

    [<Extension>]
    static member ToFSharpSet values = set values

[<Extension>]
type FSharpMap =
    static member Create([<ParamArray>] values) =
        Map.ofArray values

    [<Extension>]
    static member ToFSharpMap values = Map.ofSeq values

[<Extension>]
type Dictionary =
  [<Extension>]
  static member TryFind (d, key) = Dictionary.tryFind key d

[<Extension>]
type AsyncExtensions =
    [<Extension>]
    static member SelectMany (o, f: Func<_,_>) = 
        Async.bind f.Invoke o
        
    [<Extension>]
    static member SelectMany (o, f: Func<_,_>, mapper: Func<_,_,_>) =
        let mapper = Async.lift2 (curry mapper.Invoke)
        let v = Async.bind f.Invoke o
        mapper o v

    [<Extension>]
    static member Select (o, f: Func<_,_>) = 
        Async.map f.Invoke o
    
    [<Extension>]
    static member Run a = 
        Async.RunSynchronously a

    [<Extension>]
    static member AsyncDownloadString (web: WebClient, address: Uri) =
        web.AsyncDownloadString address

    static member FromBeginEnd (abegin: Func<_,_,_>, aend: Func<_,_>) = 
        Async.FromBeginEnd(abegin.Invoke, aend.Invoke)

type FSharpLazy = 
    static member Create (v: _ Func) = Lazy.Create v.Invoke
    static member CreateFromValue v = Lazy.CreateFromValue v