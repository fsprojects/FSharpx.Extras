namespace FSharpx

module Choice =
    open FSharpx.Collections

    /// Inject a value into the Choice type
    let returnM = Choice1Of2

    /// If Choice is 1Of2, return its value.
    /// Otherwise throw ArgumentException.
    let get =
        function
        | Choice1Of2 a -> a
        | Choice2Of2 e -> invalidArg "choice" (sprintf "The choice value was Choice2Of2 '%A'" e)
    
    /// If Choice is 1Of2, return its value.
    /// Otherwise raise the exception in 2Of2.
    let getOrRaise<'a, 'exn when 'exn :> exn> (c:Choice<'a, 'exn>) =
        match c with
        | Choice1Of2 r -> r
        | Choice2Of2 e -> raise e
    
    /// If Choice is 1Of2, return its value.
    /// Otherwise reraise the exception in 2Of2.
    let getOrReraise<'a, 'exn when 'exn :> exn> (c:Choice<'a, 'exn>) =
        match c with
        | Choice1Of2 r -> r
        | Choice2Of2 e -> reraise' e
    
    /// Wraps a function, encapsulates any exception thrown within to a Choice
    let inline protect f x = 
        try
            Choice1Of2 (f x)
        with e -> Choice2Of2 e

    /// Attempts to cast an object.
    /// Stores the cast value in 1Of2 if successful, otherwise stores the exception in 2Of2
    let inline cast (o: obj) = protect unbox o
        
    /// Sequential application
    let ap x f =
        match f,x with
        | Choice1Of2 f, Choice1Of2 x -> Choice1Of2 (f x)
        | Choice2Of2 e, _            -> Choice2Of2 e
        | _           , Choice2Of2 e -> Choice2Of2 e

    /// Sequential application
    let inline (<*>) f x = ap x f

    /// Transforms a Choice's first value by using a specified mapping function.
    let map f =
        function
        | Choice1Of2 x -> f x |> Choice1Of2
        | Choice2Of2 x -> Choice2Of2 x

    /// Infix map
    let inline (<!>) f x = map f x

    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = f <!> a <*> b

    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) a b = lift2 (fun _ z -> z) a b

    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) a b = lift2 (fun z _ -> z) a b

    /// Monadic bind
    let bind f = 
        function
        | Choice1Of2 x -> f x
        | Choice2Of2 x -> Choice2Of2 x
    
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bind f m

    /// Flipped >>=
    let inline (=<<) f m = bind f m

    /// Sequentially compose two either actions, discarding any value produced by the first
    let inline (>>.) m1 m2 = m1 >>= (fun _ -> m2)

    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g

    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    /// Maps both parts of a Choice.
    /// Applies the first function if Choice is 1Of2.
    /// Otherwise applies the second function
    let inline bimap f1 f2 = 
        function
        | Choice1Of2 x -> Choice1Of2 (f1 x)
        | Choice2Of2 x -> Choice2Of2 (f2 x)

    /// Maps both parts of a Choice.
    /// Applies the first function if Choice is 1Of2.
    /// Otherwise applies the second function
    let inline choice f1 f2 = 
        function
        | Choice1Of2 x -> f1 x
        | Choice2Of2 x -> f2 x

    /// Transforms a Choice's second value by using a specified mapping function.
    let inline mapSecond f = bimap id f

    type EitherBuilder() =
        member this.Return a = returnM a
        member this.Bind (m, f) = bind f m
        member this.ReturnFrom m = m

    let choose = EitherBuilder()

    /// If Choice is 1Of2, returns Some value. Otherwise, returns None.
    let toOption = Option.ofChoice

    /// If Some value, returns Choice1Of2 value. Otherwise, returns the supplied default value.
    let ofOption o = 
        function
        | Some a -> Choice1Of2 a
        | None -> Choice2Of2 o

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= flip f t) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

    let inline isChoice1Of2 (c:Choice<'a, 'b>) : bool =
        match c with
        | Choice1Of2 _ -> true
        | _ -> false

    let inline isChoice2Of2 (c:Choice<'a, 'b>) : bool =
        match c with
        | Choice2Of2 _ -> true
        | _ -> false

    let inline isChoice1Of3 (c:Choice<'a, 'b, 'c>) : bool =
        match c with
        | Choice1Of3 _ -> true
        | _ -> false

    let inline isChoice2Of3 (c:Choice<'a, 'b, 'c>) : bool =
        match c with
        | Choice2Of3 _ -> true
        | _ -> false

    let inline isChoice3Of3 (c:Choice<'a, 'b, 'c>) : bool =
        match c with
        | Choice3Of3 _ -> true
        | _ -> false

    let inline isChoice1Of4 (c:Choice<'a, 'b, 'c, 'd>) : bool =
        match c with
        | Choice1Of4 _ -> true
        | _ -> false

    let inline isChoice2Of4 (c:Choice<'a, 'b, 'c, 'd>) : bool =
        match c with
        | Choice2Of4 _ -> true
        | _ -> false

    let inline isChoice3Of4 (c:Choice<'a, 'b, 'c, 'd>) : bool =
        match c with
        | Choice3Of4 _ -> true
        | _ -> false

    let inline isChoice4Of4 (c:Choice<'a, 'b, 'c, 'd>) : bool =
        match c with
        | Choice4Of4 _ -> true
        | _ -> false

    let inline isChoice1Of5 (c:Choice<'a, 'b, 'c, 'd, 'e>) : bool =
        match c with
        | Choice1Of5 _ -> true
        | _ -> false

    let inline isChoice2Of5 (c:Choice<'a, 'b, 'c, 'd, 'e>) : bool =
        match c with
        | Choice2Of5 _ -> true
        | _ -> false

    let inline isChoice3Of5 (c:Choice<'a, 'b, 'c, 'd, 'e>) : bool =
        match c with
        | Choice3Of5 _ -> true
        | _ -> false

    let inline isChoice4Of5 (c:Choice<'a, 'b, 'c, 'd, 'e>) : bool =
        match c with
        | Choice4Of5 _ -> true
        | _ -> false

    let inline isChoice5Of5 (c:Choice<'a, 'b, 'c, 'd, 'e>) : bool =
        match c with
        | Choice5Of5 _ -> true
        | _ -> false

    let inline isChoice1Of6 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f>) : bool =
        match c with
        | Choice1Of6 _ -> true
        | _ -> false

    let inline isChoice2Of6 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f>) : bool =
        match c with
        | Choice2Of6 _ -> true
        | _ -> false

    let inline isChoice3Of6 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f>) : bool =
        match c with
        | Choice3Of6 _ -> true
        | _ -> false

    let inline isChoice4Of6 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f>) : bool =
        match c with
        | Choice4Of6 _ -> true
        | _ -> false

    let inline isChoice5Of6 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f>) : bool =
        match c with
        | Choice5Of6 _ -> true
        | _ -> false

    let inline isChoice6Of6 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f>) : bool =
        match c with
        | Choice6Of6 _ -> true
        | _ -> false

    let inline isChoice1Of7 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f, 'g>) : bool =
        match c with
        | Choice1Of7 _ -> true
        | _ -> false

    let inline isChoice2Of7 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f, 'g>) : bool =
        match c with
        | Choice2Of7 _ -> true
        | _ -> false

    let inline isChoice3Of7 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f, 'g>) : bool =
        match c with
        | Choice3Of7 _ -> true
        | _ -> false

    let inline isChoice4Of7 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f, 'g>) : bool =
        match c with
        | Choice4Of7 _ -> true
        | _ -> false

    let inline isChoice5Of7 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f, 'g>) : bool =
        match c with
        | Choice5Of7 _ -> true
        | _ -> false

    let inline isChoice6Of7 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f, 'g>) : bool =
        match c with
        | Choice6Of7 _ -> true
        | _ -> false

    let inline isChoice7Of7 (c:Choice<'a, 'b, 'c, 'd, 'e, 'f, 'g>) : bool =
        match c with
        | Choice7Of7 _ -> true
        | _ -> false