namespace FSharpx
#nowarn "40"

open System
open System.Collections
open System.Collections.Generic
open FSharpx.Collections

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
    /// This monad is my own and uses an 'T option. Others generally make their own Maybe<'T> type from Option<'T>.
    /// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
    type MaybeBuilder() =
        member this.Return(x) = Some x

        member this.ReturnFrom(m: 'T option) = m

        member this.Bind(m, f) = Option.bind f m

        member this.Zero() = None

        member this.Combine(m, f) = Option.bind f m

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

    /// Option wrapper monoid
    let monoid (m: _ ISemigroup) =
        { new Monoid<_>() with
            override this.Zero() = None
            override this.Combine(a, b) = 
                match a,b with
                | Some a, Some b -> Some (m.Combine(a,b))
                | Some a, None   -> Some a
                | None, Some a   -> Some a
                | None, None     -> None }
    
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
    let ofNullable (n: _ Nullable) = 
        if n.HasValue
            then Some n.Value
            else None

    /// Maps an Option to Nullable
    let toNullable =
        function
        | None -> Nullable()
        | Some x -> Nullable(x)

    /// True -> Some(), False -> None
    let inline ofBool b = if b then Some() else None

    /// Converts a function returning bool,value to a function returning value option.
    /// Useful to process TryXX style functions.
    let inline tryParseWith func = func >> function
       | true, value -> Some value
       | false, _ -> None
    
    /// If true,value then returns Some value. Otherwise returns None.
    /// Useful to process TryXX style functions.
    let inline ofBoolAndValue b = 
        match b with
        | true,v -> Some v
        | _ -> None

    /// Maps Choice 1Of2 to Some value, otherwise None.
    let ofChoice =
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

    let inline getOrElseWith v f =
        function
        | Some x -> f x
        | None -> v

    // Additional Option-Module extensions

    /// Haskell-style maybe operator
    let option (defaultValue : 'U) (map : 'T -> 'U) = function
        | None   -> defaultValue
        | Some a -> map a

    /// transforms a function in the Try...(input, out output) style
    /// into a function of type: input -> output Option
    /// Example: fromTryPattern(System.Double.TryParse)
    /// See Examples.Option
    let fromTryPattern (tryFun : ('input -> (bool * 'output))) =
        fun input ->
            match tryFun input with
            | (true,  output) -> Some output
            | (false,      _) -> None

    /// Concatenates an option of option.
    let inline concat x = 
        x >>= id

module Nullable =
    let (|Null|Value|) (x: _ Nullable) =
        if x.HasValue then Value x.Value else Null

    let create x = Nullable x
    /// Gets the value associated with the nullable or the supplied default value.
    let getOrDefault n v = match n with Value x -> x | _ -> v
    /// Gets the value associated with the nullable or the supplied default value.
    let getOrElse (n: Nullable<'T>) (v: Lazy<'T>) = match n with Value x -> x | _ -> v.Force()
    /// Gets the value associated with the Nullable.
    /// If no value, throws.
    let get (x: Nullable<_>) = x.Value
    /// Converts option to nullable
    let ofOption = Option.toNullable
    /// Converts nullable to option
    let toOption = Option.ofNullable
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

    type State<'T, 'State> = 'State -> 'T * 'State
    
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
        member this.Return(a) : State<'T,'State> = fun s -> (a,s)
        member this.ReturnFrom(m:State<'T,'State>) = m
        member this.Bind(m:State<'T,'State>, k:'T -> State<'U,'State>) : State<'U,'State> = bind k m
        member this.Zero() = this.Return ()
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        member this.TryWith(m:State<'T,'State>, h:exn -> State<'T,'State>) : State<'T,'State> =
            fun env -> try m env
                       with e -> (h e) env
        member this.TryFinally(m:State<'T,'State>, compensation) : State<'T,'State> =
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

    type Reader<'R,'T> = 'R -> 'T

    let bind k m = fun r -> (k (m r)) r
    
    /// The reader monad.
    /// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/07/much-ado-about-monads-reader-edition.aspx.
    type ReaderBuilder() =
        member this.Return(a) : Reader<'R,'T> = fun _ -> a
        member this.ReturnFrom(a:Reader<'R,'T>) = a
        member this.Bind(m:Reader<'R,'T>, k:'T -> Reader<'R,'U>) : Reader<'R,'U> = bind k m
        member this.Zero() = this.Return ()
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        member this.TryWith(m:Reader<'R,'T>, h:exn -> Reader<'R,'T>) : Reader<'R,'T> =
            fun env -> try m env
                       with e -> (h e) env
        member this.TryFinally(m:Reader<'R,'T>, compensation) : Reader<'R,'T> =
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
    
    let ask : Reader<'R,'R> = id

    let asks f = reader {
        let! r = ask
        return (f r) }

    let local (f:'r1 -> 'r2) (m:Reader<'r2,'T>) : Reader<'r1, 'T> = f >> m
    
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
    
    type History<'T> = { 
        Current: 'T
        Undos : 'T list
        Redos : 'T list }
    
    let newHistory x = { Current = x; Undos = [x]; Redos = [] }
    let current history = history.Current
    
    let getHistory = getState
    
    let putToHistory x = undoable {
        let! history = getState
        do! putState  { Current = x; 
                        Undos = history.Current :: history.Undos
                        Redos = [] } }

    let exec m s = m s |> snd |> current
    
    let getCurrent<'T> = undoable {
        let! (history:'T History) = getState
        return current history}

    let combineWithCurrent f x = undoable {
        let! currentVal = getCurrent
        do! putToHistory (f currentVal x) }
    
    let undo<'T> = undoable {
        let! (history:'T History) = getState
        match history.Undos with
        | [] -> return false
        | (x::rest) -> 
            do! putState { Current = x;
                           Undos = rest;
                           Redos = history.Current :: history.Redos }
            return true}
    
    let redo<'T> = undoable {
        let! (history:'T History) = getState
        match history.Redos with
        | [] -> return false
        | (x::rest) -> 
            do! putState { Current = x;
                           Undos = history.Current :: history.Undos;
                           Redos = rest }
            return true }

module Writer =
    open Monoid
        
    type Writer<'W, 'T> = unit -> 'T * 'W

    let bind (m: Monoid<_>) (k:'T -> Writer<'W,'U>) (writer:Writer<'W,'T>) : Writer<'W,'U> =
        fun () ->
            let (a, w) = writer()
            let (a', w') = (k a)()
            (a', m.Combine(w, w'))

    /// Inject a value into the Writer type
    let returnM (monoid: Monoid<_>) a = 
        fun () -> (a, monoid.Zero())
    
    /// The writer monad.
    /// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx.
    type WriterBuilder<'W>(monoid: 'W Monoid) =
        member this.Return(a) : Writer<'W,'T> = returnM monoid a
        member this.ReturnFrom(w:Writer<'W,'T>) = w
        member this.Bind(writer, k) = bind monoid k writer
        member this.Zero() = this.Return ()
        member this.TryWith(writer:Writer<'W,'T>, handler:exn -> Writer<'W,'T>) : Writer<'W,'T> =
            fun () -> try writer()
                      with e -> (handler e)()
        member this.TryFinally(writer, compensation) =
            fun () -> try writer()
                      finally compensation()
        member this.Using<'d,'W,'T when 'd :> IDisposable and 'd : null>(resource : 'd, body : 'd -> Writer<'W,'T>) : Writer<'W,'T> =
            this.TryFinally(body resource, fun () -> match resource with null -> () | disp -> disp.Dispose())
        member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            match guard() with
            | true -> this.Bind(m, (fun () -> this.While(guard, m))) 
            | _        -> this.Zero()
        member this.For(sequence:seq<'T>, body:'T -> Writer<'W,unit>) =
            this.Using(sequence.GetEnumerator(), 
                fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

    let writer = WriterBuilder(List.monoid<string>)

    let tell   w = fun () -> ((), w)
    let listen m = fun () -> let (a, w) = m() in ((a, w), w)
    let pass   m = fun () -> let ((a, f), w) = m() in (a, f w)
    
    let listens monoid f m = 
        let writer = WriterBuilder(monoid)
        writer {
            let! (a, b) = m
            return (a, f b) }
    
    let censor monoid (f:'w1 -> 'w2) (m:Writer<'w1,'T>) : Writer<'w2,'T> =
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

    /// Sequential application, parameterized by semigroup
    let inline apm (m: _ ISemigroup) = apa (curry m.Combine)

    type CustomValidation<'T>(semigroup: 'T ISemigroup) =
        /// Sequential application
        member this.ap x = apm semigroup x

        /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
        member this.lift2 f a b = returnM f |> this.ap a |> this.ap b

        /// Sequence actions, discarding the value of the first argument.
        member this.apr b a = this.lift2 (fun _ z -> z) a b

        /// Sequence actions, discarding the value of the second argument.
        member this.apl b a = this.lift2 (fun z _ -> z) a b

        member this.seqValidator f = 
            let inline cons a b = this.lift2 (flip List.cons) a b
            Seq.map f >> Seq.fold cons (returnM [])

        member this.sequence s =
            let inline cons a b = this.lift2 List.cons a b
            List.foldBack cons s (returnM [])

        member this.mapM f x = this.sequence (List.map f x)


    type NonEmptyListValidation<'T>() = 
        inherit CustomValidation<'T NonEmptyList>(NonEmptyList.NonEmptyListSemigroup<'T>())

    /// Sequential application
    let inline ap x = apa NonEmptyList.append x

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

#if NET40

module Task =
    open System.Threading
    open System.Threading.Tasks

    /// Task result
    type Result<'T> = 
        /// Task was canceled
        | Canceled
        /// Unhandled exception in task
        | Error of exn 
        /// Task completed successfully
        | Successful of 'T

    let run (t: unit -> Task<_>) = 
        try
            let task = t()
            task.Result |> Result.Successful
        with 
        | :? OperationCanceledException -> Result.Canceled
        | :? AggregateException as e ->
            match e.InnerException with
            | :? TaskCanceledException -> Result.Canceled
            | _ -> Result.Error e
        | e -> Result.Error e

    let toAsync (t: Task<'T>): Async<'T> =
        let abegin (cb: AsyncCallback, state: obj) : IAsyncResult = 
            match cb with
            | null -> upcast t
            | cb -> 
                t.ContinueWith(fun (_ : Task<_>) -> cb.Invoke t) |> ignore
                upcast t
        let aend (r: IAsyncResult) = 
            (r :?> Task<'T>).Result
        Async.FromBeginEnd(abegin, aend)

    /// Transforms a Task's first value by using a specified mapping function.
    let inline mapWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) f (m: Task<_>) =
        m.ContinueWith((fun (t: Task<_>) -> f t.Result), token, continuationOptions, scheduler)

    /// Transforms a Task's first value by using a specified mapping function.
    let inline map f (m: Task<_>) =
        m.ContinueWith(fun (t: Task<_>) -> f t.Result)

    let inline bindWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) (f: 'T -> Task<'U>) (m: Task<'T>) =
        m.ContinueWith((fun (x: Task<_>) -> f x.Result), token, continuationOptions, scheduler).Unwrap()

    let inline bind (f: 'T -> Task<'U>) (m: Task<'T>) = 
        m.ContinueWith(fun (x: Task<_>) -> f x.Result).Unwrap()

    let inline returnM a = 
        let s = TaskCompletionSource()
        s.SetResult a
        s.Task

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

    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = 
        a >>= fun aa -> b >>= fun bb -> f aa bb |> returnM

    /// Sequential application
    let inline ap x f = lift2 id f x

    /// Sequential application
    let inline (<*>) f x = ap x f

    /// Infix map
    let inline (<!>) f x = map f x

    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) a b = lift2 (fun _ z -> z) a b

    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) a b = lift2 (fun z _ -> z) a b
    
    type TaskBuilder(?continuationOptions, ?scheduler, ?cancellationToken) =
        let contOptions = defaultArg continuationOptions TaskContinuationOptions.None
        let scheduler = defaultArg scheduler TaskScheduler.Default
        let cancellationToken = defaultArg cancellationToken CancellationToken.None

        member this.Return x = returnM x

        member this.Zero() = returnM ()

        member this.ReturnFrom (a: Task<'T>) = a

        member this.Bind(m, f) = bindWithOptions cancellationToken contOptions scheduler f m

        member this.Combine(comp1, comp2) =
            this.Bind(comp1, comp2)

        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m(), fun () -> this.While(guard, m))

        member this.TryFinally(m, compensation) =
            try this.ReturnFrom m
            finally compensation()

        member this.Using(res: #IDisposable, body: #IDisposable -> Task<_>) =
            this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

        member this.For(sequence: seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                                 fun enum -> this.While(enum.MoveNext, fun () -> body enum.Current))

        member this.Delay (f: unit -> Task<'T>) = f

        member this.Run (f: unit -> Task<'T>) = f()

    type TaskBuilderWithToken(?continuationOptions, ?scheduler) =
        let contOptions = defaultArg continuationOptions TaskContinuationOptions.None
        let scheduler = defaultArg scheduler TaskScheduler.Default

        let lift (t: Task<_>) = fun (_: CancellationToken) -> t
        let bind (t: CancellationToken -> Task<'T>) (f: 'T -> (CancellationToken -> Task<'U>)) =
            fun (token: CancellationToken) ->
                (t token).ContinueWith((fun (x: Task<_>) -> f x.Result token), token, contOptions, scheduler).Unwrap()
        
        member this.Return x = lift (returnM x)

        member this.ReturnFrom t = lift t

        member this.ReturnFrom (t: CancellationToken -> Task<'T>) = t

        member this.Zero() = this.Return ()

        member this.Bind(t, f) = bind t f            

        member this.Bind(t, f) = bind (lift t) f                

        member this.Combine(t1, t2) = bind t1 (konst t2)        

        member this.While(guard, m) =
                if not(guard()) then 
                    this.Zero()
                else
                    bind m (fun () -> this.While(guard, m))                    

        member this.TryFinally(t : CancellationToken -> Task<'T>, compensation) =
            try t
            finally compensation()

        member this.Using(res: #IDisposable, body: #IDisposable -> (CancellationToken -> Task<'T>)) =
            this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

        member this.For(sequence: seq<'T>, body) =            
                this.Using(sequence.GetEnumerator(),
                                 fun enum -> this.While(enum.MoveNext, fun token -> body enum.Current token))
        
        member this.Delay f = this.Bind(this.Return (), f)

#endif
