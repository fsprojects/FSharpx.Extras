namespace FSharpx

module Result =
    open FSharpx.Collections

    /// Inject a value into the Result type
    let returnM = Ok
    /// If Result is Ok, return its value.
    /// Otherwise throw ArgumentException.
    let get =
        function
        | Ok a -> a
        | Error e -> invalidArg "result" (sprintf "The result value was Error '%A'" e)
    /// Wraps a function, encapsulates any exception thrown within to a Result
    let inline protect f x =
        try
            Ok (f x)
        with e -> Error e
    /// Attempts to cast an object.
    /// Stores the cast value in Ok if successful, otherwise stores the exception in Error
    let inline cast (o: obj) = protect unbox o

    /// Sequential application
    let ap x f =
        match f,x with
        | Ok f        , Ok x    -> Ok (f x)
        | Error e     , _       -> Error e
        | _           , Error e -> Error e

    /// Sequential application
    let inline (<*>) f x = ap x f

    /// Infix map
    let inline (<!>) f x = Result.map f x

    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = f <!> a <*> b

    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) a b = lift2 (fun _ z -> z) a b

    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) a b = lift2 (fun z _ -> z) a b

    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = Result.bind f m

    /// Flipped >>=
    let inline (=<<) f m = Result.bind f m

    /// Sequentially compose two either actions, discarding any value produced by the first
    let inline (>>.) m1 m2 = m1 >>= (fun _ -> m2)

    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g

    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    /// Maps both parts of a Choice.
    /// Applies the first function if Result is Ok.
    /// Otherwise applies the second function
    let inline bimap f1 f2 =
        function
        | Ok x -> Ok (f1 x)
        | Error x -> Error (f2 x)

    /// If Some value, returns Ok value. Otherwise, returns the supplied default value.
    let ofOption o =
        function
        | Some a -> Ok a
        | None -> Error o

    /// If Some value, returns Ok value. Otherwise, returns the supplied default value from a function.
    let ofOptionF f =
        function
        | Some a -> Ok a
        | None -> Error (f())

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    /// Gets the value of result if the result is Ok, otherwise evaluates f and returns the result.
    let defaultWith (f:unit->'a) (result:Result<'a,_>) : 'a =
        match result with
        | Ok    x -> x
        | Error _ -> f()

    /// Gets the value of result if the result is Ok, otherwise returns the specified default value v.
    let defaultValue (v:'a) (result:Result<'a,_>) : 'a =
        match result with
        | Ok    x -> x
        | Error _ -> v

    /// Case analysis for the Result type. If the value is Ok x, apply the first function to x; if it is Error e, apply the second function to e.
    let inline either (f:'a->'c) (h:'b->'c) (result: Result<'a,'b>) : 'c =
        match result with
        | Ok    x -> f x
        | Error e -> h e

    type ResultBuilder() =
        member _.Return x = Ok x
        member _.Bind (m, f) = Result.bind f m
        member _.ReturnFrom m = m
        member _.Zero() = Ok ()
        member _.Delay f = f
        member _.Run f = f()

        member this.TryWith(m, h) =
            try this.ReturnFrom(m)
            with e -> h e

        member this.TryFinally(m, compensation) =
            try this.ReturnFrom(m)
            finally compensation()

        member this.Using(res:#System.IDisposable, body) =
            this.TryFinally(body res, fun () -> if not (isNull (box res)) then res.Dispose())


        member this.While(guard, f) =
            if not (guard()) then
                this.Zero()
            else
                f() |> ignore
                this.While(guard, f)

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

    let result = ResultBuilder()
