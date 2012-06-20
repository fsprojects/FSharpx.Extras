namespace FSharpx
#nowarn "40"

open System
open System.Collections
open System.Collections.Generic

module Monoid =
    
    /// Monoid (associative binary operation with identity)
    /// The monoid implementation comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx.
    [<AbstractClass>]
    type Monoid<'a>() =
        /// <summary>
        /// Identity
        /// </summary>
        abstract member mempty : 'a

        /// <summary>
        /// Associative operation
        /// </summary>
        abstract member mappend : 'a -> 'a -> 'a

        /// <summary>
        /// Fold a list using this monoid
        /// </summary>
        abstract member mconcat : 'a seq -> 'a
        default x.mconcat a = Seq.fold x.mappend x.mempty a
    
    /// List monoid
    type ListMonoid<'a>() =
        inherit Monoid<'a list>()
            override this.mempty = []
            override this.mappend a b = a @ b

    /// Option wrapper monoid
    type OptionMonoid<'a>(m: 'a Monoid) =
        inherit Monoid<'a option>()
            override this.mempty = None
            override this.mappend a b = 
                match a,b with
                | Some a, Some b -> Some (m.mappend a b)
                | Some a, None   -> Some a
                | None, Some a   -> Some a
                | None, None     -> None
                
    /// Monoid (int,0,+)
    let IntSumMonoid = 
        { new Monoid<int>() with
            override this.mempty = 0
            override this.mappend a b = a + b }

    /// Monoid (int,1,*)
    let IntProductMonoid =
        { new Monoid<int>() with
            override this.mempty = 1
            override this.mappend a b = a * b }

/// Generic monadic operators    
module Operators =

    /// Inject a value into the monadic type
    let inline returnM builder x = (^M: (member Return: 'b -> 'c) (builder, x))
    let inline bindM builder m f = (^M: (member Bind: 'd -> ('e -> 'c) -> 'c) (builder, m, f))
    let inline liftM builder f m =
        let inline ret x = returnM builder (f x)
        bindM builder m ret

    /// Sequential application
    let inline applyM (builder1:^M1) (builder2:^M2) f m =
        bindM builder1 f <| fun f' ->
            bindM builder2 m <| fun m' ->
                returnM builder2 (f' m') 

module Async =
    open Operators
    
    /// Sequentially compose two actions, passing any value produced by the second as an argument to the first.
    let inline bind f m = async.Bind(m,f)
    /// Inject a value into the async type
    let inline returnM x = returnM async x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM async m f
    /// Flipped >>=
    let inline (=<<) f m = bindM async m f
    /// Sequential application
    let inline (<*>) f m = applyM async async f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Flipped map
    let inline pipe m f = liftM async f m
    let inline pipe2 x y f = returnM f <*> x <*> y
    let inline pipe3 x y z f = returnM f <*> x <*> y <*> z
    /// Transforms an async value by using a specified mapping function.
    let inline map f m = pipe m f
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f x y = returnM f <*> x <*> y
    /// Infix map
    let inline (<!>) f m = pipe m f
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = pipe2 x y (fun _ z -> z)
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = pipe2 x y (fun z _ -> z)

    /// Sequentially compose two async actions, discarding any value produced by the first
    let inline (>>.) m f = bindM async m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

module ZipList = 
    let returnM v = Seq.initInfinite (fun _ -> v)
    /// Sequential application
    let (<*>) f a = Seq.zip f a |> Seq.map (fun (k,v) -> k v)
    /// Sequential application
    let inline ap m f = f <*> m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y

module Option =

    /// The maybe monad.
    /// This monad is my own and uses an 'a option. Others generally make their own Maybe<'a> type from Option<'a>.
    /// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
    type MaybeBuilder() =
        member this.Return(x) = Some x
        member this.ReturnFrom(m: 'a option) = m
        member this.Bind(m, f) = Option.bind f m
        member this.Zero() = Some()
        member this.Combine(m, f: unit -> _) = Option.bind f m
        member this.Delay(f: unit -> _) = f
        member this.Run(f) = f()
        member this.TryWith(m, h) =
            try this.ReturnFrom(m)
            with e -> h e
        member this.TryFinally(m, compensation) =
            try this.ReturnFrom(m)
            finally compensation()
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
        member this.While(guard, f) =
            if not (guard()) then this.Zero() else
            this.Bind(f(), fun _ -> this.While(guard, f))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
    let maybe = MaybeBuilder()
    
    open Operators
    
    /// Inject a value into the option type
    let inline returnM x = returnM maybe x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM maybe m f
    /// Flipped >>=
    let inline (=<<) f m = bindM maybe m f
    /// Sequential application
    let inline (<*>) f m = applyM maybe maybe f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Infix map
    let inline (<!>) f m = Option.map f m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y

    /// Sequentially compose two maybe actions, discarding any value produced by the first
    let inline (>>.) m f = bindM maybe m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x
    /// Maps a Nullable to Option
    let fromNullable (n: _ Nullable) = 
        if n.HasValue
            then Some n.Value
            else None
    /// Maps an Option to Nullable
    let toNullable =
        function
        | None -> Nullable()
        | Some x -> Nullable(x)

    /// True -> Some(), False -> None
    let inline fromBool b = if b then Some() else None

    /// Converts a function returning bool,value to a function returning value option.
    /// Useful to process TryXX style functions.
    let inline tryParseWith func = func >> function
       | true, value -> Some value
       | false, _ -> None
    
    /// If true,value then returns Some value. Otherwise returns None.
    /// Useful to process TryXX style functions.
    let inline fromBoolAndValue b = 
        match b with
        | true,v -> Some v
        | _ -> None

    /// Maps Choice 1Of2 to Some value, otherwise None.
    let fromChoice =
        function
        | Choice1Of2 a -> Some a
        | _ -> None

    /// Gets the value associated with the option or the supplied default value.
    let inline getOrElse v =
        function
        | Some x -> x
        | None -> v

    /// Gets the value associated with the option or the supplied default value.
    let inline getOrElseLazy (v: _ Lazy) =
        function
        | Some x -> x
        | None -> v.Value

    /// Gets the value associated with the option or the supplied default value from a function.
    let inline getOrElseF v =
        function
        | Some x -> x
        | None -> v()

    /// Gets the value associated with the option or the default value for the type.
    let getOrDefault =
        function
        | Some x -> x
        | None -> Unchecked.defaultof<_>
    
    /// Gets the option if Some x, otherwise the supplied default value.
    let inline orElse v =
        function
        | Some x -> Some x
        | None -> v

    /// Applies a predicate to the option. If the predicate returns true, returns Some x, otherwise None.
    let inline filter pred =
        function
        | Some x when pred x -> Some x
        | _ -> None

    /// Attempts to cast an object. Returns None if unsuccessful.
    [<CompiledName("Cast")>]
    let inline cast (o: obj) =
        try
            Some (unbox o)
        with _ -> None

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

module Nullable =
    let (|Null|Value|) (x: _ Nullable) =
        if x.HasValue then Value x.Value else Null

    let create x = Nullable x
    /// Gets the value associated with the nullable or the supplied default value.
    let getOrDefault n v = match n with Value x -> x | _ -> v
    /// Gets the value associated with the nullable or the supplied default value.
    let getOrElse (n: 'a Nullable) (v: 'a Lazy) = match n with Value x -> x | _ -> v.Force()
    /// Gets the value associated with the Nullable.
    /// If no value, throws.
    let get (x: _ Nullable) = x.Value
    /// Converts option to nullable
    let fromOption = Option.toNullable
    /// Converts nullable to option
    let toOption = Option.fromNullable
    /// Monadic bind
    let bind f x =
        match x with
        | Null -> Nullable()
        | Value v -> f v
    /// True if Nullable has value
    let hasValue (x: _ Nullable) = x.HasValue
    /// True if Nullable does not have value
    let isNull (x: _ Nullable) = not x.HasValue
    /// Returns 1 if Nullable has value, otherwise 0
    let count (x: _ Nullable) = if x.HasValue then 1 else 0
    /// Evaluates the equivalent of List.fold for a nullable.
    let fold f state x =
        match x with
        | Null -> state
        | Value v -> f state v
    /// Performs the equivalent of the List.foldBack operation on a nullable.
    let foldBack f x state =
        match x with
        | Null -> state
        | Value v -> f x state
    /// Evaluates the equivalent of List.exists for a nullable.
    let exists p x =
        match x with
        | Null -> false
        | Value v -> p x
    /// Evaluates the equivalent of List.forall for a nullable.
    let forall p x = 
        match x with
        | Null -> true
        | Value v -> p x
    /// Executes a function for a nullable value.
    let iter f x =
        match x with
        | Null -> ()
        | Value v -> f v
    /// Transforms a Nullable value by using a specified mapping function.
    let map f x =
        match x with
        | Null -> Nullable()
        | Value v -> Nullable(f v)
    /// Convert the nullable to an array of length 0 or 1.
    let toArray x = 
        match x with
        | Null -> [||]
        | Value v -> [| v |]
    /// Convert the nullable to a list of length 0 or 1.
    let toList x =
        match x with
        | Null -> []
        | Value v -> [v]
        
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let lift2 f (a: _ Nullable) (b: _ Nullable) =
        if a.HasValue && b.HasValue
            then Nullable(f a.Value b.Value)
            else Nullable()

    let mapBool op a b =
        match a,b with
        | Value x, Value y -> op x y
        | _ -> false

    let inline (+?) a b = (lift2 (+)) a b
    let inline (-?) a b = (lift2 (-)) a b
    let inline ( *?) a b = (lift2 ( *)) a b
    let inline (/?) a b = (lift2 (/)) a b
    let inline (>?) a b = (mapBool (>)) a b
    let inline (>=?) a b = a >? b || a = b
    let inline (<?) a b = (mapBool (<)) a b
    let inline (<=?) a b = a <? b || a = b
    let inline notn (a: bool Nullable) = 
        if a.HasValue 
            then Nullable(not a.Value) 
            else Nullable()
    let inline (&?) a b = 
        let rec and' a b = 
            match a,b with
            | Null, Value y when not y -> Nullable(false)
            | Null, Value y when y -> Nullable()
            | Null, Null -> Nullable()
            | Value x, Value y -> Nullable(x && y)
            | _ -> and' b a
        and' a b

    let inline (|?) a b = notn ((notn a) &? (notn b))

    type Int32 with
        member x.n = Nullable x

    type Double with
        member x.n = Nullable x

    type Single with
        member x.n = Nullable x

    type Byte with
        member x.n = Nullable x

    type Int64 with
        member x.n = Nullable x

    type Decimal with
        member x.n = Nullable x

module State =

    type State<'a, 's> = 's -> 'a * 's
    
    let getState = fun s -> (s,s)
    let putState s = fun _ -> ((),s)
    let eval m s = m s |> fst
    let exec m s = m s |> snd
    let empty = fun s -> ((), s)
    let bind k m = fun s -> let (a, s') = m s in (k a) s'
    
    /// The state monad.
    /// The algorithm is adjusted from my original work off of Brian Beckman's http://channel9.msdn.com/shows/Going+Deep/Brian-Beckman-The-Zen-of-Expressing-State-The-State-Monad/.
    /// The approach was adjusted from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2009/12/30/much-ado-about-monads-state-edition.aspx and mirrors his final result.
    type StateBuilder() =
        member this.Return(a) : State<'a,'s> = fun s -> (a,s)
        member this.ReturnFrom(m:State<'a,'s>) = m
        member this.Bind(m:State<'a,'s>, k:'a -> State<'b,'s>) : State<'b,'s> = bind k m
        member this.Zero() = this.Return ()
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        member this.TryWith(m:State<'a,'s>, h:exn -> State<'a,'s>) : State<'a,'s> =
            fun env -> try m env
                       with e -> (h e) env
        member this.TryFinally(m:State<'a,'s>, compensation) : State<'a,'s> =
            fun env -> try m env
                       finally compensation()
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, (fun () -> this.While(guard, m)))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
    let state = new StateBuilder()
    
    open Operators

    /// Inject a value into the State type
    let inline returnM x = returnM state x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM state m f
    /// Flipped >>=
    let inline (=<<) f m = bindM state m f
    /// Sequential application
    let inline (<*>) f m = applyM state state f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Transforms a State value by using a specified mapping function.
    let inline map f m = liftM state f m
    /// Infix map
    let inline (<!>) f m = map f m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    /// Sequentially compose two state actions, discarding any value produced by the first
    let inline (>>.) m f = bindM state m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

module Reader =

    type Reader<'r,'a> = 'r -> 'a

    let bind k m = fun r -> (k (m r)) r
    
    /// The reader monad.
    /// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/07/much-ado-about-monads-reader-edition.aspx.
    type ReaderBuilder() =
        member this.Return(a) : Reader<'r,'a> = fun _ -> a
        member this.ReturnFrom(a:Reader<'r,'a>) = a
        member this.Bind(m:Reader<'r,'a>, k:'a -> Reader<'r,'b>) : Reader<'r,'b> = bind k m
        member this.Zero() = this.Return ()
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        member this.TryWith(m:Reader<'r,'a>, h:exn -> Reader<'r,'a>) : Reader<'r,'a> =
            fun env -> try m env
                       with e -> (h e) env
        member this.TryFinally(m:Reader<'r,'a>, compensation) : Reader<'r,'a> =
            fun env -> try m env
                       finally compensation()
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, (fun () -> this.While(guard, m)))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
    let reader = new ReaderBuilder()
    
    let ask : Reader<'r,'r> = id
    let asks f = reader {
        let! r = ask
        return (f r) }
    let local (f:'r1 -> 'r2) (m:Reader<'r2,'a>) : Reader<'r1, 'a> = f >> m
    
    open Operators
    
    /// Inject a value into the Reader type
    let inline returnM x = returnM reader x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM reader m f
    /// Flipped >>=
    let inline (=<<) f m = bindM reader m f
    /// Sequential application
    let inline (<*>) f m = applyM reader reader f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Transforms a Reader value by using a specified mapping function.
    let inline map f m = liftM reader f m
    /// Infix map
    let inline (<!>) f m = map f m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    /// Sequentially compose two reader actions, discarding any value produced by the first
    let inline (>>.) m f = bindM reader m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

module Undo =
    // UndoMonad on top of StateMonad
    open State
    
    let undoable = state
    
    type 'a History = { 
        Current:'a
        Undos : 'a list
        Redos : 'a list }
    
    let newHistory x = { Current = x; Undos = [x]; Redos = [] }
    let current history = history.Current
    
    let getHistory = getState
    
    let putToHistory x = undoable {
        let! history = getState
        do! putState  { Current = x; 
                        Undos = history.Current :: history.Undos
                        Redos = [] } }

    let exec m s = m s |> snd |> current
    
    let getCurrent<'a> = undoable {
        let! (history:'a History) = getState
        return current history}

    let combineWithCurrent f x = undoable {
        let! currentVal = getCurrent
        do! putToHistory (f currentVal x) }
    
    let undo<'a> = undoable {
        let! (history:'a History) = getState
        match history.Undos with
        | [] -> return false
        | (x::rest) -> 
            do! putState { Current = x;
                           Undos = rest;
                           Redos = history.Current :: history.Redos }
            return true}
    
    let redo<'a> = undoable {
        let! (history:'a History) = getState
        match history.Redos with
        | [] -> return false
        | (x::rest) -> 
            do! putState { Current = x;
                           Undos = history.Current :: history.Undos;
                           Redos = rest }
            return true }

module Writer =
    open Monoid
        
    type Writer<'w, 'a> = unit -> 'a * 'w

    let bind (m: _ Monoid) (k:'a -> Writer<'w,'b>) (writer:Writer<'w,'a>) : Writer<'w,'b> =
        fun () ->
            let (a, w) = writer()
            let (a', w') = (k a)()
            (a', m.mappend w w')

    /// Inject a value into the Writer type
    let returnM (monoid: _ Monoid) a = 
        fun () -> (a, monoid.mempty)
    
    /// The writer monad.
    /// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx.
    type WriterBuilder<'w>(monoid: 'w Monoid) =
        member this.Return(a) : Writer<'w,'a> = returnM monoid a
        member this.ReturnFrom(w:Writer<'w,'a>) = w
        member this.Bind(writer, k) = bind monoid k writer
        member this.Zero() = this.Return ()
        member this.TryWith(writer:Writer<'w,'a>, handler:exn -> Writer<'w,'a>) : Writer<'w,'a> =
            fun () -> try writer()
                      with e -> (handler e)()
        member this.TryFinally(writer, compensation) =
            fun () -> try writer()
                      finally compensation()
        member this.Using<'d,'w,'a when 'd :> IDisposable and 'd : null>(resource : 'd, body : 'd -> Writer<'w,'a>) : Writer<'w,'a> =
            this.TryFinally(body resource, fun () -> match resource with null -> () | disp -> disp.Dispose())
        member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            match guard() with
            | true -> this.Bind(m, (fun () -> this.While(guard, m))) 
            | _        -> this.Zero()
        member this.For(sequence:seq<'a>, body:'a -> Writer<'w,unit>) =
            this.Using(sequence.GetEnumerator(), 
                fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

    let writer = WriterBuilder(Monoid.ListMonoid<string>())

    let tell   w = fun () -> ((), w)
    let listen m = fun () -> let (a, w) = m() in ((a, w), w)
    let pass   m = fun () -> let ((a, f), w) = m() in (a, f w)
    
    let listens monoid f m = 
        let writer = WriterBuilder(monoid)
        writer {
            let! (a, b) = m
            return (a, f b) }
    
    let censor monoid (f:'w1 -> 'w2) (m:Writer<'w1,'a>) : Writer<'w2,'a> =
        let writer = WriterBuilder(monoid)
        writer { let! a = m
                 return (a, f)
               } |> pass

    open Operators
    
    let inline private ret x = returnM writer x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM writer m f
    /// Flipped >>=
    let inline (=<<) f m = bindM writer m f
    /// Sequential application
    let inline (<*>) f m = applyM writer writer f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Transforms a Writer value by using a specified mapping function.
    let inline map f m = liftM writer f m
    /// Infix map
    let inline (<!>) f m = map f m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = ret f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    /// Sequentially compose two state actions, discarding any value produced by the first
    let inline (>>.) m f = bindM writer m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (ret s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (ret [])

    let inline mapM f x = sequence (List.map f x)

module Choice =
    /// Inject a value into the Choice type
    let returnM = Choice1Of2

    /// If Choice is 1Of2, return its value.
    /// Otherwise throw ArgumentException.
    let get =
        function
        | Choice1Of2 a -> a
        | Choice2Of2 e -> invalidArg "choice" (sprintf "The choice value was Choice2Of2 '%A'" e)
    
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
        member this.Bind(m,f) = bind f m

    /// If Choice is 1Of2, returns Some value. Otherwise, returns None.
    let toOption = Option.fromChoice

    /// If Some value, returns Choice1Of2 value. Otherwise, returns the supplied default value.
    let fromOption o = 
        function
        | Some a -> Choice1Of2 a
        | None -> Choice2Of2 o

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)
        // pointfree:
        //Seq.fold (flip f >> bind |> flip) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

module Validation =
    open Choice
    open Monoid

    let (|Success|Failure|) = 
        function
        | Choice1Of2 a -> Success a
        | Choice2Of2 e -> Failure e

    /// Sequential application, parameterized by append
    let apa append x f = 
        match f,x with
        | Choice1Of2 f, Choice1Of2 x     -> Choice1Of2 (f x)
        | Choice2Of2 e, Choice1Of2 x     -> Choice2Of2 e
        | Choice1Of2 f, Choice2Of2 e     -> Choice2Of2 e
        | Choice2Of2 e1, Choice2Of2 e2 -> Choice2Of2 (append e1 e2)

    /// Sequential application, parameterized by monoid
    let inline apm (m: _ Monoid) = apa m.mappend

    type CustomValidation<'a>(monoid: 'a Monoid) =
        /// Sequential application
        member this.ap x = apm monoid x
        /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
        member this.lift2 f a b = returnM f |> this.ap a |> this.ap b
        /// Sequence actions, discarding the value of the first argument.
        member this.apr b a = this.lift2 (fun _ z -> z) a b
        /// Sequence actions, discarding the value of the second argument.
        member this.apl b a = this.lift2 (fun z _ -> z) a b

    /// Sequential application
    let inline ap x = apa List.append x

    /// Sequential application
    let inline (<*>) f x = ap x f
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b

    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y

    let seqValidator f = 
        let inline cons a b = lift2 (flip List.cons) a b
        Seq.map f >> Seq.fold cons (returnM [])

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

module Continuation =

    /// The continuation monad.
    /// The algorithm is from Wes Dyer http://blogs.msdn.com/b/wesdyer/archive/2008/01/11/the-marvels-of-monads.aspx.
    /// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
    /// Current implementation from Matt's gist at https://gist.github.com/628956
    type Cont<'a,'r> = ('a -> 'r) -> (exn -> 'r) -> 'r
    
    let private protect f x cont econt =
        let res = try Choice1Of2 (f x) with err -> Choice2Of2 err
        match res with
        | Choice1Of2 v -> cont v
        | Choice2Of2 v -> econt v
    
    let runCont (c:Cont<_,_>) cont econt = c cont econt
    let throw exn : Cont<'a,'r> = fun cont econt -> econt exn
    let callcc (f: ('a -> Cont<'b,'r>) -> Cont<'a,'r>) : Cont<'a,'r> =
        fun cont econt -> runCont (f (fun a -> (fun _ _ -> cont a))) cont econt
    let bind f comp1 = 
        fun cont econt ->
            runCont comp1 (fun a -> protect f a (fun comp2 -> runCont comp2 cont econt) econt) econt     

    type ContinuationBuilder() =
        member this.Return(a) : Cont<_,_> = fun cont econt -> cont a
        member this.ReturnFrom(comp:Cont<_,_>) = comp
        member this.Bind(comp1, f) = bind f comp1
        member this.Catch(comp:Cont<_,_>) : Cont<Choice<_, exn>, _> = fun cont econt ->
            runCont comp (fun v -> cont (Choice1Of2 v)) (fun err -> cont (Choice2Of2 err))
        member this.Zero() =
            this.Return ()
        member this.TryWith(tryBlock, catchBlock) =
            this.Bind(this.Catch tryBlock, (function Choice1Of2 v -> this.Return v 
                                                   | Choice2Of2 exn -> catchBlock exn))
        member this.TryFinally(tryBlock, finallyBlock) =
            this.Bind(this.Catch tryBlock, (function Choice1Of2 v -> finallyBlock(); this.Return v 
                                                   | Choice2Of2 exn -> finallyBlock(); throw exn))
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Combine(comp1, comp2) = this.Bind(comp1, (fun () -> comp2))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(pred, body) =
            if pred() then this.Bind(body, (fun () -> this.While(pred,body))) else this.Return ()
        member this.For(items:seq<_>, body) =
            this.Using(items.GetEnumerator(),
                (fun enum -> this.While((fun () -> enum.MoveNext()), this.Delay(fun () -> body enum.Current))))
    let cont = ContinuationBuilder()
    
    open Operators
    
    /// Inject a value into the Cont type
    let inline returnM x = returnM cont x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM cont m f
    /// Flipped >>=
    let inline (=<<) f m = bindM cont m f
    /// Sequential application
    let inline (<*>) f m = applyM cont cont f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Transforms a Cont value by using a specified mapping function.
    let inline map f m = liftM cont f m
    /// Infix map
    let inline (<!>) f m = map f m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    /// Sequentially compose two continuation actions, discarding any value produced by the first
    let inline (>>.) m f = bindM cont m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

    /// The coroutine type from http://fssnip.net/7M
    type Coroutine() =
        let tasks = new Queue<Cont<unit,unit>>()

        member this.Put(task) =
            let withYield = cont {
                do! callcc <| fun exit ->
                    task <| fun () ->
                    callcc <| fun c ->
                    tasks.Enqueue(c())
                    exit()
                if tasks.Count <> 0 then
                    do! tasks.Dequeue() }
            tasks.Enqueue(withYield)
            
        member this.Run() =
            runCont (tasks.Dequeue()) ignore raise

module Distribution =
    
    type 'a Outcome = {
        Value: 'a
        Probability : BigRational    }
    
    type 'a Distribution = 'a Outcome seq
    
    // P(A AND B) = P(A | B) * P(B)
    let bind (f: 'a -> 'b Distribution) (dist:'a Distribution) =
        dist 
        |> Seq.map (fun p1 -> 
            f p1.Value
            |> Seq.map (fun p2 -> 
                { Value = p2.Value; 
                    Probability = 
                        p1.Probability * p2.Probability}))
        |> Seq.concat : 'b Distribution
    
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) dist f = bind f dist
    /// Flipped >>=
    let inline (=<<) f dist = bind f dist
    
    /// Inject a value into the Distribution type
    let returnM (value:'a) =     
        Seq.singleton { Value = value ; Probability = 1N/1N }
            : 'a Distribution
    
    type DistributionMonadBuilder() =
        member this.Bind (r, f) = bind f r
        member this.Return x = returnM x
        member this.ReturnFrom x = x
    
    let distribution = DistributionMonadBuilder()
    
    // Create some helpers
    let toUniformDistribution seq : 'a Distribution =
        let l = Seq.length seq
        seq 
        |> Seq.map (fun e ->
            { Value = e; 
                Probability = 1N / bignum.FromInt l })
    
    let probability (dist:'a Distribution) = 
        dist
        |> Seq.map (fun o -> o.Probability)
        |> Seq.sum
    
    let certainly = returnM
    let impossible<'a> :'a Distribution = toUniformDistribution []
    
    let fairDice sides = toUniformDistribution [1..sides]
    
    type CoinSide = 
        | Heads 
        | Tails
    
    let fairCoin = toUniformDistribution [Heads; Tails]
    
    let filter predicate (dist:'a Distribution) : 'a Distribution =
        dist |> Seq.filter (fun o -> predicate o.Value)
    
    let filterInAnyOrder items dist =
        items
        |> Seq.fold (fun d item -> filter (Seq.exists ((=) (item))) d) dist

    /// Transforms a Distribution value by using a specified mapping function.
    let map f (dist:'a Distribution) : 'b Distribution = 
        dist 
        |> Seq.map (fun o -> { Value = f o.Value; Probability = o.Probability })
    
    let selectOne values =
        [for e in values -> e,values |> Seq.filter ((<>) e)] 
        |> toUniformDistribution
    
    let rec selectMany n values =
        match n with 
        | 0 -> certainly ([],values)
        | _ -> 
            distribution {
                let! (x,c1) = selectOne values
                let! (xs,c2) = selectMany (n-1) c1
                return x::xs,c2}
            
    let select n values = 
        selectMany n values     
        |> map (fst >> List.rev)
    
    let remove items = Seq.filter (fun v -> Seq.forall ((<>) v) items)
