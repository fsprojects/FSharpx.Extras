namespace FSharpx

open System
open System.IO
open System.Net
open System.Collections.Generic
open System.Runtime.CompilerServices
open Microsoft.FSharp.Control.StreamReaderExtensions
open Microsoft.FSharp.Control.WebExtensions
open FSharpx.Collections
open FSharpx.DataStructures

/// Helps the C# compiler with Func type inference.
type L =
    /// Helps the C# compiler with Func type inference.
    static member F (f: Func<_>) = f
    /// Helps the C# compiler with Func type inference.
    static member F (f: Func<_,_>) = f
    /// Helps the C# compiler with Func type inference.
    static member F (f: Func<_,_,_>) = f

/// <summary>
/// Conversion functions from Action/Func to FSharpFunc
/// We need these because FuncConvert often makes C# type inference fail.
/// </summary>
[<Extension>]
type FSharpFunc =
    /// Convert an Action into an F# function returning unit
    static member FromAction (f: Action) =
        fun () -> f.Invoke()
    
    /// Convert an Action into an F# function returning unit
    static member FromAction (f: Action<_>) =
        fun x -> f.Invoke x

    /// Convert an Action into an F# function returning unit
    static member FromAction (f: Action<_,_>) =
        fun x y -> f.Invoke(x,y)

    /// Convert an Action into an F# function returning unit
    static member FromAction (f: Action<_,_,_>) =
        fun x y z -> f.Invoke(x,y,z)

    /// Convert a Func into an F# function
    static member FromFunc (f: Func<_>) =
        fun () -> f.Invoke()

    /// Convert a Func into an F# function
    static member FromFunc (f: Func<_,_>) =
        fun x -> f.Invoke x

    /// Convert a Func into an F# function
    static member FromFunc (f: Func<_,_,_>) =
        fun x y -> f.Invoke(x,y)

/// Extensions around Actions and Funcs
[<Extension>]
type Funcs =
    /// Converts an action to a function returning Unit
    [<Extension>]
    static member ToFunc (a: Action) =
        Func<_>(a.Invoke)

    /// Converts an action to a function returning Unit
    [<Extension>]
    static member ToFunc (a: Action<_>) =
        Func<_,_>(a.Invoke)
  
    /// Converts an action to a function returning Unit
    [<Extension>]
    static member ToFunc (a: Action<_,_>) =
        Func<_,_,_>(curry a.Invoke)

    /// Converts an action to a function returning Unit
    [<Extension>]
    static member ToFunc (f: Action<_,_,_>) =
        Func<_,_,_,_>(fun a b c -> f.Invoke(a,b,c))

    /// Converts an uncurried function to a curried function
    [<Extension>]
    static member Curry (f: Func<_,_,_>) =
        Func<_,Func<_,_>>(fun a -> Func<_,_>(fun b -> f.Invoke(a,b)))

    /// Converts an uncurried function to a curried function
    [<Extension>]
    static member Curry (f: Func<_,_,_,_>) =
        Func<_,Func<_,Func<_,_>>>(fun a -> Func<_,Func<_,_>>(fun b -> Func<_,_>(fun c -> f.Invoke(a,b,c))))

    /// Converts an action with 2 arguments into an action taking a 2-tuple
    [<Extension>]
    static member Tuple (f: Action<_,_>) =
        Action<_>(fun (a,b) -> f.Invoke(a,b))

    /// Converts an action with 3 arguments into an action taking a 3-tuple
    [<Extension>]
    static member Tuple (f: Action<_,_,_>) =
        Action<_>(fun (a,b,c) -> f.Invoke(a,b,c))

    /// Converts an action with 4 arguments into an action taking a 4-tuple
    [<Extension>]
    static member Tuple (f: Action<_,_,_,_>) =
        Action<_>(fun (a,b,c,d) -> f.Invoke(a,b,c,d))

    /// Converts an action taking a 2-tuple into an action with 2 parameters
    [<Extension>]
    static member Untuple (f: Action<_ * _>) =
        Action<_,_>(fun a b -> f.Invoke(a,b))

    /// /// Converts an action taking a 3-tuple into an action with 3 parameters
    [<Extension>]
    static member Untuple (f: Action<_ * _ * _>) =
        Action<_,_,_>(fun a b c -> f.Invoke(a,b,c))

    /// Converts an action taking a 4-tuple into an action with 4 parameters
    [<Extension>]
    static member Untuple (f: Action<_ * _ * _ * _>) =
        Action<_,_,_,_>(fun a b c d -> f.Invoke(a,b,c,d))

    /// Composes two functions.
    /// Mathematically: f . g
    [<Extension>]
    static member Compose (f: Func<_,_>, g: Func<_,_>) =
        Func<_,_>(fun x -> f.Invoke(g.Invoke(x)))

    /// Composes two functions (forward composition).
    /// Mathematically: g . f
    [<Extension>]
    static member AndThen (f: Func<_,_>, g: Func<_,_>) =
        Func<_,_>(fun x -> g.Invoke(f.Invoke(x)))

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

    /// Gets the option if Some x, otherwise the supplied default value.
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
    static member GetOrDefault (o: Option<_>) =
        match o with
        | Some x -> x
        | _ -> Unchecked.defaultof<_>

    [<Extension>]
    static member ToFSharpChoice (o, other) =
        match o with
        | Some v -> Choice1Of2 v
        | _ -> Choice2Of2 other

    /// Converts the option to a list of length 0 or 1
    [<Extension>]
    static member ToFSharpList o = Option.toList o

    /// Converts the option to an array of length 0 or 1
    [<Extension>]
    static member ToArray o = Option.toArray o

    /// Transforms an option value by using a specified mapping function
    [<Extension>]
    static member Select (o, f: Func<_,_>) = Option.map f.Invoke o

    /// Invokes a function on an optional value that itself yields an option
    [<Extension>]
    static member SelectMany (o, f: Func<_,_>) = Option.bind f.Invoke o

    /// Invokes a function on an optional value that itself yields an option,
    /// and then applies a mapping function
    [<Extension>]
    static member SelectMany (o, f: Func<_,_>, mapper: Func<_,_,_>) =
      let mapper = Option.lift2 (curry mapper.Invoke)
      let v = Option.bind f.Invoke o
      mapper o v

    /// <summary>
    /// Evaluates the equivalent of <see cref="System.Linq.Enumerable.Aggregate"/> for an option
    /// </summary>
    [<Extension>]
    static member Aggregate (o, state, f: Func<_,_,_>) =
        Option.fold (curry f.Invoke) state o

    /// Applies a predicate to the option. If the predicate returns true, returns Some x, otherwise None.
    [<Extension>]
    static member Where (o: _ option, pred: _ Predicate) =
      Option.filter pred.Invoke o

    [<Extension>]
    static member Sequence o = Option.sequence o

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

    /// If Choice is 1Of2, return its value.
    /// Otherwise throw ArgumentException.
    [<Extension>]
    static member Value (c: Choice<_,_>) = Choice.get c

    /// Attempts to cast an object.
    /// Stores the cast value in 1Of2 if successful, otherwise stores the exception in 2Of2
    static member Cast (o: obj) = Choice.cast o

    [<Extension>]
    static member ToFSharpOption c = Option.ofChoice c

    [<Extension>]
    static member Try (f: Func<_,_>) = Func<_,_>(Choice.protect f.Invoke)

    [<Extension>]
    static member Try (f: Func<_,_>, v) = Choice.protect f.Invoke v

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
    static member Join (c: Choice<'a, 'e NonEmptyList>, inner: Choice<'b, 'e NonEmptyList>, outerKeySelector: Func<'a,'c>, innerKeySelector: Func<'b,'c>, resultSelector: Func<'a,'b,'d>) =
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

    [<Extension>]
    static member Sequence c = Choice.sequence c

    // validation

    static member Error (x: string) = Choice2Of2 (NonEmptyList.singleton x)
    static member Errors x : Choice<_, string NonEmptyList> = Choice2Of2 x
    static member Errors (error, [<ParamArray>] errors) : Choice<_, string NonEmptyList> = Choice2Of2 (NonEmptyList.create error (Array.toList errors))
    static member Ok x : Choice<_, string NonEmptyList> = Choice1Of2 x

    static member Validator (p: _ Predicate, errorMsg: string) =
        let v x = 
            if p.Invoke x
                then FSharpChoice.Ok x
                else FSharpChoice.Error errorMsg
        Func<_,_>(v)

    [<Extension>]
    static member ApValidation (f: Choice<Func<_,_>, _>, x) =
        f 
        |> Choice.map (fun a -> a.Invoke)
        |> Validation.ap x

    [<Extension>]
    static member SequenceValidation s = Validation.sequence s

    [<Extension>]
    static member SelectMValidation (x, f: Func<_,_>) = Validation.mapM f.Invoke x

    [<Extension>]
    static member ReturnValidation x : Choice<_, string NonEmptyList> = Choice1Of2 x

    static member EnumerableValidator (f: Func<'a, Choice<'a, _ NonEmptyList>>) : Func<'a seq, Choice<'a seq, _ NonEmptyList>> =
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
type NonEmptyListEx =
    [<Extension>]
    static member Concat(x,y) = NonEmptyList.append x y

    [<Extension>]
    static member Concat(x,y) = NonEmptyList.appendList x y

    [<Extension>]
    static member Cons(list, head) = NonEmptyList.cons head list

    [<Extension>]
    static member Select(list, mapper: Func<_,_>) = NonEmptyList.map mapper.Invoke list

[<Extension>]
type Dictionary =
  [<Extension>]
  static member TryFind (d, key) = Dictionary.tryFind key d

[<Extension>]
type EnumerableEx =
    [<Extension>]
    static member FirstOrNone source = Seq.tryHead source

[<Extension>]
type FSharpAsyncEx =
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

    /// Ignores (discards) the result of the async computation
    [<Extension>]
    static member IgnoreResult a =
        Async.Ignore a

    /// Encapsulates any possible exceptions during bind in a Choice
    [<Extension>]
    static member Protect a =
        Async.Catch a
    
    [<Extension>]
    static member Run a = 
        Async.RunSynchronously a

    [<Extension>]
    static member Start a = 
        Async.Start a

    [<Extension>]
    static member FSharpAsyncDownloadString (web: WebClient, address: Uri) =
        web.AsyncDownloadString address

    [<Extension>]
    static member FSharpAsyncGetResponse (w: WebRequest) =
        w.AsyncGetResponse()

    static member FromBeginEnd (abegin: Func<_,_,_>, aend: Func<_,_>) = 
        Async.FromBeginEnd(abegin.Invoke, aend.Invoke)

    [<Extension>]
    static member ToFSharpAsync (f: Func<_>) =
        Async.FromBeginEnd(f.BeginInvoke, f.EndInvoke)

    static member Return a = async.Return a

    [<Extension>]
    static member Parallel a = Async.Parallel a

    [<Extension>]
    static member FSharpAsyncReadToEnd (s: StreamReader) =
        s.AsyncReadToEnd()

type FSharpLazy = 
    static member Create (v: _ Func) = Lazy.Create v.Invoke
    static member CreateFromValue v = Lazy.CreateFromValue v

[<Extension>]
type RoseTree =
    static member Singleton x = RoseTree.singleton x
    static member Create(root, [<ParamArray>] children) = RoseTree.create root (LazyList.ofArray children)
    static member Create(root, children) = RoseTree.create root children

    [<Extension>]
    static member Select(tree, f: Func<_,_>) = RoseTree.map f.Invoke tree

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>) = RoseTree.bind f.Invoke o

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>, mapper: Func<_,_,_>) =
        let mapper = RoseTree.lift2 (curry mapper.Invoke)
        let v = RoseTree.bind f.Invoke o
        mapper o v

    [<Extension>]
    static member SelectAccum (o, state, f: Func<_,_,_>) =
        let ff = FSharpFunc.FromFunc f
        RoseTree.mapAccum ff state o
