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
        
    let inline bind f m = async.Bind(m,f)
    let inline returnM x = returnM async x
    let inline (>>=) m f = bindM async m f
    let inline (=<<) f m = bindM async m f
    /// Sequential application
    let inline (<*>) f m = applyM async async f m
    /// Sequential application
    let inline ap m f = f <*> m
    let inline pipe m f = liftM async f m
    let inline pipe2 x y f = returnM f <*> x <*> y
    let inline pipe3 x y z f = returnM f <*> x <*> y <*> z
    let inline map f m = pipe m f
    let inline map2 f x y = returnM f <*> x <*> y
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


module ZipList = 
    let returnM v = Seq.initInfinite (fun _ -> v)
    /// Sequential application
    let (<*>) f a = Seq.zip f a |> Seq.map (fun (k,v) -> k v)
    /// Sequential application
    let inline ap m f = f <*> m
    let inline map2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = map2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = map2 (fun z _ -> z) x y

module Option =

    /// The maybe monad.
    /// This monad is my own and uses an 'a option. Others generally make their own Maybe<'a> type from Option<'a>.
    /// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
    type MaybeBuilder() =
        member this.Return(x) = Some x
        member this.ReturnFrom(m: 'a option) = m
        member this.Bind(m, f) = Option.bind f m
        member this.Zero() = None
        member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)
        member this.Delay(f) = this.Bind(this.Return(), f)
        member this.TryWith(m, h) = this.ReturnFrom(m)
        member this.TryFinally(m, compensation) =
            try this.ReturnFrom(m)
            finally compensation()
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, fun () -> this.While(guard, m))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
    let maybe = MaybeBuilder()
    
    open Operators
    
    let inline returnM x = returnM maybe x
    let inline (>>=) m f = bindM maybe m f
    let inline (=<<) f m = bindM maybe m f
    /// Sequential application
    let inline (<*>) f m = applyM maybe maybe f m
    /// Sequential application
    let inline ap m f = f <*> m
    let inline (<!>) f m = Option.map f m
    let inline map2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = map2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = map2 (fun z _ -> z) x y

    /// Sequentially compose two maybe actions, discarding any value produced by the first
    let inline (>>.) m f = bindM maybe m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let fromNullable (n: _ Nullable) = 
        if n.HasValue
            then Some n.Value
            else None
    let toNullable =
        function
        | None -> Nullable()
        | Some x -> Nullable(x)

    let inline fromBool b = if b then Some() else None

    let inline fromBoolAndValue b = 
        match b with
        | true,v -> Some v
        | _ -> None

    let fromChoice =
        function
        | Choice1Of2 a -> Some a
        | _ -> None

    let inline getOrElse v =
        function
        | Some x -> x
        | None -> v

    let inline getOrElseLazy (v: _ Lazy) =
        function
        | Some x -> x
        | None -> v.Value

    let inline getOrElseF v =
        function
        | Some x -> x
        | None -> v()

    let getOrDefault =
        function
        | Some x -> x
        | None -> Unchecked.defaultof<_>
        
    let inline orElse v =
        function
        | Some x -> Some x
        | None -> v

    let inline filter pred =
        function
        | Some x when pred x -> Some x
        | _ -> None

    [<CompiledName("Cast")>]
    let inline cast (o: obj) =
        try
            Some (unbox o)
        with _ -> None

module Nullable =
    let (|Null|Value|) (x: _ Nullable) =
        if x.HasValue then Value x.Value else Null

    let create x = Nullable x
    let getOrDefault n v = match n with Value x -> x | _ -> v
    let getOrElse (n: 'a Nullable) (v: 'a Lazy) = match n with Value x -> x | _ -> v.Force()
    let get (x: _ Nullable) = x.Value
    let fromOption = Option.toNullable
    let toOption = Option.fromNullable
    let bind f x =
        match x with
        | Null -> Nullable()
        | Value v -> f v
    let hasValue (x: _ Nullable) = x.HasValue
    let isNull (x: _ Nullable) = not x.HasValue
    let count (x: _ Nullable) = if x.HasValue then 1 else 0
    let fold f state x =
        match x with
        | Null -> state
        | Value v -> f state v
    let foldBack f x state =
        match x with
        | Null -> state
        | Value v -> f x state
    let exists p x =
        match x with
        | Null -> false
        | Value v -> p x
    let forall p x = 
        match x with
        | Null -> true
        | Value v -> p x
    let iter f x =
        match x with
        | Null -> ()
        | Value v -> f v
    let map f x =
        match x with
        | Null -> Nullable()
        | Value v -> Nullable(f v)
    let toArray x = 
        match x with
        | Null -> [||]
        | Value v -> [| v |]
    let toList x =
        match x with
        | Null -> []
        | Value v -> [v]
        
    let map2 f (a: _ Nullable) (b: _ Nullable) =
        if a.HasValue && b.HasValue
            then Nullable(f a.Value b.Value)
            else Nullable()

    let mapBool op a b =
        match a,b with
        | Value x, Value y -> op x y
        | _ -> false

    let inline (+?) a b = (map2 (+)) a b
    let inline (-?) a b = (map2 (-)) a b
    let inline ( *?) a b = (map2 ( *)) a b
    let inline (/?) a b = (map2 (/)) a b
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
    
    let inline returnM x = returnM state x
    let inline (>>=) m f = bindM state m f
    let inline (=<<) f m = bindM state m f
    /// Sequential application
    let inline (<*>) f m = applyM state state f m
    /// Sequential application
    let inline ap m f = f <*> m
    let inline map f m = liftM state f m
    let inline (<!>) f m = map f m
    let inline map2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = map2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = map2 (fun z _ -> z) x y
    /// Sequentially compose two state actions, discarding any value produced by the first
    let inline (>>.) m f = bindM state m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x


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
    
    let inline returnM x = returnM reader x
    let inline (>>=) m f = bindM reader m f
    let inline (=<<) f m = bindM reader m f
    /// Sequential application
    let inline (<*>) f m = applyM reader reader f m
    /// Sequential application
    let inline ap m f = f <*> m
    let inline map f m = liftM reader f m
    let inline (<!>) f m = map f m
    let inline map2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = map2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = map2 (fun z _ -> z) x y
    /// Sequentially compose two reader actions, discarding any value produced by the first
    let inline (>>.) m f = bindM reader m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x


module Undo =
    // UndoMonad on top of StateMonad
    open State
    
    let undoable = state
    
    type 'a History = { 
        Current:'a
        Undos : 'a list
        Redos : 'a list }
    
    let empty x = { Current = x; Undos = []; Redos = [] }
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

module Choice =
    let returnM = Choice1Of2

    let get =
        function
        | Choice1Of2 a -> a
        | Choice2Of2 e -> invalidArg "choice" (sprintf "The choice value was Choice2Of2 '%A'" e)

    let inline cast (o: obj) =
        try 
            Choice1Of2 (unbox o)
        with e -> Choice2Of2 e
        
    let ap x f =
        match f,x with
        | Choice1Of2 f, Choice1Of2 x -> Choice1Of2 (f x)
        | Choice2Of2 e, _            -> Choice2Of2 e
        | _           , Choice2Of2 e -> Choice2Of2 e

    /// Sequential application
    let inline (<*>) f x = ap x f

    let map f =
        function
        | Choice1Of2 x -> f x |> Choice1Of2
        | Choice2Of2 x -> Choice2Of2 x

    let inline (<!>) f x = map f x
    let inline map2 f a b = f <!> a <*> b

    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) a b = map2 (fun _ z -> z) a b
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) a b = map2 (fun z _ -> z) a b

    let bind f = 
        function
        | Choice1Of2 x -> f x
        | Choice2Of2 x -> Choice2Of2 x
    
    let inline (>>=) m f = bind f m
    let inline (=<<) f m = bind f m
    /// Sequentially compose two either actions, discarding any value produced by the first
    let inline (>>.) m1 m2 = m1 >>= (fun _ -> m2)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let bimap f1 f2 = 
        function
        | Choice1Of2 x -> Choice1Of2 (f1 x)
        | Choice2Of2 x -> Choice2Of2 (f2 x)

    let inline mapSecond f = bimap id f

    type EitherBuilder() =
        member this.Return a = returnM a
        member this.Bind(m,f) = bind f m

    let toOption = Option.fromChoice
    let fromOption o = 
        function
        | Some a -> Choice1Of2 a
        | None -> Choice2Of2 o

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
        member this.map2 f a b = returnM f |> this.ap a |> this.ap b
        /// Sequence actions, discarding the value of the first argument.
        member this.apr b a = this.map2 (fun _ z -> z) a b
        /// Sequence actions, discarding the value of the second argument.
        member this.apl b a = this.map2 (fun z _ -> z) a b

    let private stringListValidation = CustomValidation(ListMonoid<string>())

    /// Sequential application
    let ap = stringListValidation.ap

    /// Sequential application
    let inline (<*>) f x = ap x f
    let map2 = stringListValidation.map2

    /// Sequence actions, discarding the value of the first argument.
    let ( *>) = stringListValidation.apr
    /// Sequence actions, discarding the value of the first argument.
    let ( <*) = stringListValidation.apl

    let seqValidator f = 
        let zero = returnM []
        Seq.map f >> Seq.fold (map2 (flip FSharpx.List.cons)) zero


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
    
    let inline returnM x = returnM cont x
    let inline (>>=) m f = bindM cont m f
    let inline (=<<) f m = bindM cont m f
    /// Sequential application
    let inline (<*>) f m = applyM cont cont f m
    /// Sequential application
    let inline ap m f = f <*> m
    let inline map f m = liftM cont f m
    let inline (<!>) f m = map f m
    let inline map2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = map2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = map2 (fun z _ -> z) x y
    /// Sequentially compose two continuation actions, discarding any value produced by the first
    let inline (>>.) m f = bindM cont m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x


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
    
    let inline (>>=) dist f = bind f dist
    let inline (=<<) f dist = bind f dist
    
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

(*
# Iteratee

The *Iteratee* module, part of the [FSharpx](http://github.com/fsharp/fsharpx) library, provides a set of types and functions for building compositional, input processing components.

## System.IO.Stream-based processing

The System.IO.Stream type should be familiar to anyone who has ever worked with I/O in .NET. Streams are the primary abstraction available for working with streams of data, whether over the file system (FileStream) or network protocols (NetworkStream). Streams also have a nice support structure in the form of TextReaders and TextWriters, among other, similar types.

A common scenario for I/O processing is parsing an HTTP request message. Granted, most will rely on ASP.NET, HttpListener, or WCF to do this for them. However, HTTP request parsing has a lot of interesting elements that are useful for demonstrating problem areas in inefficient resource usage using other techniques. For our running sample, we'll focus on parsing the headers of the following HTTP request message:

    let httpRequest : byte [] = @"GET /some/uri HTTP/1.1
    Accept:text/html,application/xhtml+xml,application/xml
    Accept-Charset:ISO-8859-1,utf-8;q=0.7,*;q=0.3
    Accept-Encoding:gzip,deflate,sdch
    Accept-Language:en-US,en;q=0.8
    Cache-Control:max-age=0
    Connection:keep-alive
    Host:stackoverflow.com
    If-Modified-Since:Sun, 25 Sep 2011 20:55:29 GMT
    Referer:http://www.bing.com/search?setmkt=en-US&q=don't+use+IEnumerable%3Cbyte%3E
    User-Agent:Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.4 (KHTML, like Gecko) Chrome/16.0.889.0 Safari/535.4

    <!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 4.01//EN"" ""http://www.w3.org/TR/html4/strict.dtd"">
    <html>
    <head>
    ...
    </head>
    <body>
    ...
    </body>
    </html>"B

Using the standard Stream processing apis, you might write something like the following:

    let rec readConsecutiveLines (reader:System.IO.StreamReader) cont =
        if reader.EndOfStream then cont []
        else let line = reader.ReadLine()
             if System.String.IsNullOrEmpty(line) then cont []
             else readConsecutiveLines reader (fun tail -> cont (line::tail))

    let readFromStream() =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let result =
            [ for _ in 1..10000 do 
                use stream = new System.IO.MemoryStream(httpRequest)
                use reader = new System.IO.StreamReader(stream)
                yield readConsecutiveLines reader id ]
        sw.Stop()
        printfn "Stream read %d in %d ms" result.Length sw.ElapsedMilliseconds

    readFromStream()

What might be the problems with this approach?

1. Blocking the current thread

This is a synchronous use of reading from a Stream. In fact, `StreamReader` can only be used in a synchronous fashion. There are no methods that even offer the Asynchronous Programming Model (APM). For that, you'll need to read from the `Stream` in chunks and find the line breaks yourself. We'll get to more on this in a few minutes.

2. Poor composition

First off, the sample above is performing side effects throughout, and side-effects don't compose. You might suggest using standard function composition to create a complete message parser, but what if you don't know ahead of time what type of request body you'll receive? Can you pause processing to select the appropriate processor? You could certainly address this with some conditional branching, but where exactly do you tie that into your current logic? Also, the current parser breaks on lines using the StreamReader's built in logic. If you want to do parsing on individual lines, where would you tie in that logic?

3. Memory consumption

In this example, we are not taking any care about our memory use. To that end, you might argue that we should just use `StreamReader.ReadToEnd()` and then break up the string using a compositional parser like [FParsec](http://http://www.quanttec.com/fparsec/). If we don't care about careful use of memory, that's actually quite a good idea. However, in most network I/O situations -- such as writing high-performance servers -- you really want to control your memory use very carefully. `StreamReader` does allow you to specify the chunk size, so it's not a complete case against using `StreamReader`, which is why this is a last-place argument. And of course, you can certainly go after `Stream.Read()` directly.

## IEnumerable-based processing

How can we add better compositionality and refrain from blocking the thread? One way others have solved this problem is through the use of iterators. A common example can be found on [Stack Overflow](http://stackoverflow.com/questions/2630359/convert-stream-to-ienumerable-if-possible-when-keeping-laziness). Iterators allow you to publish a lazy stream of data, either one element at a time or in chunks, and through LINQ or F#'s `Seq` module, chain together processors for the data.

    // Read the stream byte by byte
    let readStreamByByte (stream: System.IO.Stream) =
        seq { while true do
                let x = stream.ReadByte()
                if (int x) < 0 then ()
                else yield x }

    // Read the stream by chunks
    let readStreamByChunk chunkSize (stream: System.IO.Stream) =
        let buffer = Array.zeroCreate<byte> chunkSize
        seq { while true do
                let bytesRead = stream.Read(buffer, 0, chunkSize)
                if bytesRead = 0 then ()
                else yield buffer }

    // When you are sure you want text by lines
    let readStreamByLines (reader: System.IO.StreamReader) =
        seq { while not reader.EndOfStream do
                yield reader.ReadLine() }

Three options are presented. In each, I'm using a `Stream` or `StreamReader`, but you could just as easily replace those with a `byte[]`, `ArraySegment<byte>`, or `SocketAsyncEventArgs`. What could be wrong with these options?

1. Lazy, therefore resource contentious

Iterators are pull-based, so you'll only retrieve a chunk when requested. This sounds pretty good for your processing code, but it's not a very good situation for your sockets. If you have data coming in, you want to get it moved out as quickly as possible to free up your allocated threads and pinned memory buffers for more incoming or outgoing traffic.

2. Lazy, therefore non-deterministic

Each of these items is lazy; therefore, it's impossible to know when you can free up resources. Who owns the `Stream` or `StreamReader` passed into each method? When is it safe to free the resource? If used immediately within a function, you may be fine, but you'd also be just as well off if you used a `list` or `array` comprehension and blocked until all bytes were read. (The one possible exception might be when using a [co-routine style async pattern](http://tomasp.net/blog/csharp-async.aspx).)

## A fold, by any other name

Looking back to our example of parsing the headers of an HTTP request message, we most likely want to return not just a set of strings but an abstract syntax tree represented as an F# discriminated union. A perfect candidate for taking in our iterator above and producing the desired result is our old friend [`Seq.fold`](http://msdn.microsoft.com/en-us/library/ee353471.aspx).

    val fold : ('State -> 'a -> 'State) -> 'State -> seq<'a> -> 'State

The left fold is a very useful function. It equivalent to the [`Enumerable.Aggregate`](http://msdn.microsoft.com/en-us/library/system.linq.enumerable.aggregate.aspx) extension method in LINQ. This function takes in allows for the incremental creation of a result starting with a seed value.

Looks like we're done here. However, there are still problems with stopping here:

1. Composition

You can certainly use function composition to generate an appropriate state incrementing function, but you would still have the problem of being able to pause to delay selecting the appropriate message body processor.

2. Early termination

Suppose you really only ever want just the message headers. How would you stop processing to free your resources as quickly as possible? Forget it. Fold is going to keep going until it runs out of chunks. Even if you have your fold function stop updating the state after the headers are complete, you won't get a result until the entire data stream has been processed.

Finally, we still haven't addressed the original issues with iterators.

## Iteratees

The iteratee itself is a data consumer. It consumes data in chunks until it has either consumed enough data to produce a result or receives an EOF.

An iteratee is based on the `fold` operator with two differences:

1. The iteratee may complete its processing early by returning a Done state. The iteratee may return the unused portion of any chunk it was passed in its Done state. This should not be confused with the rest of the stream not yet provided by the enumerator.

2. The iteratee does not pull data but receives chunks of data from an "enumerator". It returns a continuation to receive and process the next chunk. This means that an iteratee may be paused at any time, either by neglecting to pass it any additional data or by passing an Empty chunk.

*)
module Iteratee =
    open FSharpx
    
    /// A stream of chunks of data generated by an Enumerator.
    /// The stream can be composed of chunks of 'a, empty blocks indicating a wait, or an EOF marker.
    /// Be aware that when using #seq<_> types, you will need to check for both Seq.empty ([]) and Empty.
    type Stream<'a> =
        | Chunk of 'a
        | Empty
        | EOF
    
    /// The iteratee is a stream consumer that will consume a stream of data until either 
    /// it receives an EOF or meets its own requirements for consuming data. The iteratee
    /// will return Continue whenever it is ready to receive the next chunk. An iteratee
    /// is fed data by an Enumerator, which generates a Stream. 
    type Iteratee<'el,'a> =
        | Done of 'a * Stream<'el>
        | Continue of (Stream<'el> -> Iteratee<'el,'a>)
    
    /// An enumerator generates a stream of data and feeds an iteratee, returning a new iteratee.
    type Enumerator<'el,'a> = Iteratee<'el,'a> -> Iteratee<'el,'a>
    
    /// An enumeratee is an enumerator that produces an iteratee using another iteratee as input.
    /// Enumeratees can be used for tasks such as encoding or encrypting data.
    type Enumeratee<'elo,'eli,'a> = Iteratee<'eli,'a> -> Iteratee<'elo, Iteratee<'eli,'a>>
    
    [<AutoOpen>]
    module Primitives =
    
        let returnI x = Done(x,Empty)
        let empty<'a> : Iteratee<'a,_> = Done((),Empty)
        let doneI x s = Done(x,s)
        let continueI k = Continue k
    
        let rec bind f m =
            match m with
            | Done(x, extra) ->
                match f x with
                | Done(x',_) -> Done(x', extra)
                | Continue k -> k extra
            | Continue k -> Continue(bind f << k)
    
        let tryFinally compensation i =
            let rec step = function 
                | Continue k -> Continue(fun s -> step (k s))
                | i -> compensation(); i
            in step i
    
        let rec enumEOF = function 
            | Done(x,_) -> Done(x, EOF)
            | Continue k ->
                match k EOF with
                | Continue _ -> failwith "enumEOF: divergent iteratee"
                | i -> enumEOF i
        
        let run i =
            match enumEOF i with
            | Done(x,_) -> x
            | Continue _ -> failwith "run: divergent iteratee"
        
    type IterateeBuilder() =
        member this.Return(x) = Done(x, Empty)
        member this.ReturnFrom(m:Iteratee<_,_>) = m
        member this.Bind(m, f) = bind f m
        member this.Zero() = empty<_>
        member this.Combine(comp1, comp2) = bind (fun () -> comp2) comp1
        member this.Delay(f) = bind f empty<_>
        member this.TryFinally(m, compensation) = tryFinally compensation m
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, (fun () -> this.While(guard, m)))
        member this.For(sequence:#seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
    let iteratee = IterateeBuilder()
    
    let inline returnM x = returnI x
    let inline (>>=) m f = bind f m
    let inline (=<<) f m = bind f m
    /// Sequential application
    let inline (<*>) f m = f >>= fun f' -> m >>= fun m' -> returnM (f' m')
    /// Sequential application
    let inline ap m f = f <*> m
    let inline map f m = m >>= fun x -> returnM (f x)
    let inline (<!>) f m = map f m
    let inline map2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = map2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = map2 (fun z _ -> z) x y
    /// Sequentially compose two iteratee actions, discarding any value produced by the first
    let inline (>>.) m f = m >>= (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x
    
    module List =
        open Operators
        
        let fold step seed =
            let f = Microsoft.FSharp.Collections.List.fold step
            let rec loop acc = function
                | Empty -> Continue (loop acc)
                | Chunk [] -> Continue (loop acc)
                | Chunk xs -> Continue (loop (f acc xs))
                | EOF -> Done(acc, EOF)
            Continue (loop seed)
        
        let length<'a> : Iteratee<'a list, int> =
            let rec step n = function
                | Empty | Chunk [] -> Continue (step n)
                | Chunk x -> Continue (step (n + x.Length))
                | EOF -> Done(n, EOF)
            in Continue (step 0)
        
        let peek<'a> : Iteratee<'a list, 'a option> =
            let rec inner =
                let rec step = function
                    | Empty | Chunk ([]:'a list) -> inner
                    | Chunk(x::xs) as s -> Done(Some x, s)
                    | EOF -> Done(None, EOF)
                Continue step
            in inner
        
        let head<'a> : Iteratee<'a list, 'a option> =
            let rec inner =
                let rec step = function
                    | Empty | Chunk ([]:'a list) -> inner
                    | Chunk(x::xs) -> Done(Some x, Chunk xs)
                    | EOF -> Done(None, EOF)
                Continue step
            in inner
        
        let drop n =
            let rec step n = function
                | Empty | Chunk [] -> Continue <| step n
                | Chunk str ->
                    if str.Length < n then
                        Continue <| step (n - str.Length)
                    else let extra = List.skip n str in Done((), Chunk extra)
                | EOF -> Done((), EOF)
            in if n <= 0 then empty<_> else Continue (step n)
        
        let private dropWithPredicate pred listOp =
            let rec step = function
                | Empty | Chunk [] -> Continue step
                | Chunk x ->
                    match listOp pred x with
                    | [] -> Continue step
                    | x' -> Done((), Chunk x')
                | EOF as s -> Done((), s)
            in Continue step

        let dropWhile pred = dropWithPredicate pred List.skipWhile
        let dropUntil pred = dropWithPredicate pred List.skipUntil
        
        let take n =
            let rec step before n = function
                | Empty | Chunk [] -> Continue <| step before n
                | Chunk str ->
                    if str.Length < n then
                        Continue <| step (before @ str) (n - str.Length)
                    else let str', extra = List.splitAt n str in Done(before @ str', Chunk extra)
                | EOF -> Done(before, EOF)
            in if n <= 0 then Done([], Empty) else Continue (step [] n)
        
        let private takeWithPredicate (pred:'a -> bool) listOp =
            let rec step before = function
                | Empty | Chunk [] -> Continue (step before)
                | Chunk str ->
                    match listOp pred str with
                    | str', [] -> Continue (step (before @ str'))
                    | str', extra -> Done(before @ str', Chunk extra)
                | EOF -> Done(before, EOF)
            in Continue (step [])
        
        let takeWhile pred = takeWithPredicate pred List.span
        let takeUntil pred = takeWithPredicate pred List.split
        
        let heads str =
            let rec loop count str =
                match count, str with
                | (count, []) -> Done(count, EOF)
                | (count, str) -> Continue (step count str)
            and step count str s =
                match str, s with
                | str, Empty -> loop count str
                | str, (Chunk []) -> loop count str
                | c::t, (Chunk (c'::t')) ->
                    if c = c' then step (count + 1) t (Chunk t') 
                    else Done(count, Chunk (c'::t'))
                | _, s -> Done(count, s)
            loop 0 str
        
        let readLines =
            let toString chars = String(Array.ofList chars)
            let newlines = ['\r';'\n']
            let newline = ['\n']
            let isNewline c = c = '\r' || c = '\n'
            let terminators = heads newlines >>= fun n -> if n = 0 then heads newline else Done(n, Empty)
            let rec lines acc = takeUntil isNewline >>= fun l -> terminators >>= check acc l
            and check acc l count =
                match l, count with
                | _, 0 -> Done(Choice1Of2 (List.rev acc |> List.map toString), Chunk l)
                | [], _ -> Done(Choice2Of2 (List.rev acc |> List.map toString), EOF)
                | l, _ -> lines (l::acc)
            lines []
        
        (* ========= Enumerators ========= *)
        
        // val enumeratePure1Chunk :: 'a list -> Enumerator<'a list,'b>
        let enumeratePure1Chunk str i =
            match str, i with 
            | [], _ -> i
            | _, Done(_,_) -> i
            | _::_, Continue k -> k (Chunk str)
        
        // val enumeratePureNChunk :: 'a list -> int -> Enumerator<'a list,'b>
        let rec enumeratePureNChunk n str i =
            match str, i with
            | [], _ -> i
            | _, Done(_,_) -> i
            | _::_, Continue k ->
                let x, xs = List.splitAt n str in enumeratePureNChunk n xs (k (Chunk x))

        //val enumerate :: 'a list -> Enumerator<'a list,'b>
        let rec enumerate str i = 
            match str, i with
            | [], _ -> i
            | _, Done(_,_) -> i
            | x::xs, Continue k -> enumerate xs (k (Chunk [x]))

    module Binary =
        open Operators

        (* ========= Iteratees ========= *)

        let fold step seed =
            let f = ByteString.fold step
            let rec loop acc = function
                | Empty -> Continue (loop acc)
                | Chunk xs when ByteString.isEmpty xs -> Continue (loop acc)
                | Chunk xs -> Continue (loop (f acc xs))
                | EOF -> Done(acc, EOF)
            Continue (loop seed)

        let length = 
            let rec step n = function
                | Empty -> Continue (step n)
                | Chunk x when ByteString.isEmpty x -> Continue (step n)
                | Chunk x -> Continue (step (n + x.Count))
                | EOF as s -> Done(n, s)
            Continue (step 0)

        let rec peek =
            let rec step = function
                | Empty -> peek
                | Chunk x when ByteString.isEmpty x -> peek
                | Chunk x as s -> Done(Some(ByteString.head x), s)
                | s -> Done(None, s)
            Continue step

        let rec head =
            let rec step = function
                | Empty -> head 
                | Chunk x when ByteString.isEmpty x -> head
                | Chunk x -> Done(Some(ByteString.head x), Chunk(ByteString.tail x))
                | s -> Done(None, s)
            Continue step

        let drop n =
            let rec step n = function
                | Empty -> Continue <| step n
                | Chunk str when ByteString.isEmpty str -> Continue <| step n
                | Chunk str ->
                    if ByteString.length str < n then
                        Continue <| step (n - (ByteString.length str))
                    else let extra = ByteString.skip n str in Done((), Chunk extra)
                | EOF -> Done((), EOF)
            in if n <= 0 then empty<_> else Continue <| step n

        let private dropWithPredicate pred byteStringOp =
            let rec step = function
                | Empty -> Continue step
                | Chunk x when ByteString.isEmpty x -> Continue step
                | Chunk x ->
                    let x' = byteStringOp pred x in
                    if ByteString.isEmpty x' then Continue step
                    else Done((), Chunk x')
                | s -> Done((), s)
            Continue step

        let dropWhile pred = dropWithPredicate pred ByteString.skipWhile
        let dropUntil pred = dropWithPredicate pred ByteString.skipUntil

        let take n =
            let rec step before n = function
                | Empty -> Continue <| step before n
                | Chunk str when ByteString.isEmpty str -> Continue <| step before n
                | Chunk str ->
                    if ByteString.length str < n then
                        Continue <| step (ByteString.append before str) (n - (ByteString.length str))
                    else let str', extra = ByteString.splitAt n str in Done(ByteString.append before str', Chunk extra)
                | EOF -> Done(before, EOF)
            in if n <= 0 then Done(ByteString.empty, Empty) else Continue (step ByteString.empty n)

        let private takeWithPredicate (pred:'a -> bool) byteStringOp =
            let rec step before = function
                | Empty -> Continue <| step before
                | Chunk str when ByteString.isEmpty str -> Continue <| step before
                | Chunk str ->
                    match byteStringOp pred str with
                    | str', extra when ByteString.isEmpty extra -> Continue <| step (ByteString.append before str')
                    | str', extra -> Done(ByteString.append before str', Chunk extra)
                | EOF -> Done(before, EOF)
            Continue (step ByteString.empty)

        let takeWhile pred = takeWithPredicate pred ByteString.span
        let takeUntil pred = takeWithPredicate pred ByteString.split

        let heads str =
            let rec loop count str =
                if ByteString.isEmpty str then Done(count, EOF)
                else Continue (step count str)
            and step count str = function
                | Empty -> loop count str
                | Chunk x when ByteString.isEmpty x -> loop count str
                | Chunk x when not (ByteString.isEmpty str) ->
                    let c, t = ByteString.head str, ByteString.tail str
                    let c', t' = ByteString.head x, ByteString.tail x
                    if c = c' then step (count + 1) t (Chunk t') 
                    else Done(count, Chunk x)
                | s -> Done(count, s)
            loop 0 str

        let readLines =
            let crlf = ByteString.create "\r\n"B
            let lf = ByteString.singleton '\n'B
            let isNewline c = c = '\r'B || c = '\n'B
            let terminators = heads crlf >>= fun n -> if n = 0 then heads lf else Done(n, Empty)
            let rec lines acc = takeUntil isNewline >>= fun bs -> terminators >>= check acc bs
            and check acc bs count =
                if count = 0 then
                    Done(Choice1Of2 (List.rev acc |> List.map ByteString.toString), Chunk bs)
                elif ByteString.isEmpty bs then
                    Done(Choice2Of2 (List.rev acc |> List.map ByteString.toString), EOF)
                else lines (bs::acc)
            lines []

        (* ========= Enumerators ========= *)

        // val enumeratePure1Chunk :: ByteString -> Enumerator<ByteString,'b>
        let enumeratePure1Chunk str i =
            if ByteString.isEmpty str then i
            else match i with
                 | Done(_,_) -> i
                 | Continue k -> k (Chunk str)

        // val enumeratePureNChunk :: ByteString -> int -> Enumerator<ByteString,'b>
        let rec enumeratePureNChunk n str i =
            if ByteString.isEmpty str then i
            else match i with
                 | Done(_,_) -> i
                 | Continue k -> let s1, s2 = ByteString.splitAt n str in enumeratePureNChunk n s2 (k (Chunk s1))

        // val enumerate :: ByteString -> Enumerator<ByteString,'b>
        let rec enumerate str i =
            if ByteString.isEmpty str then i
            else match i with
                 | Done(_,_) -> i
                 | Continue k -> let x, xs = ByteString.head str, ByteString.tail str in enumerate xs (k (Chunk (ByteString.singleton x)))

        let enumStream bufferSize (stream:#System.IO.Stream) i =
            let buffer = Array.zeroCreate<byte> bufferSize
            let rec step = function Continue k -> read k | i -> i
            and read k =
                let result = stream.Read(buffer, 0, bufferSize) in
                if result = 0 then Continue k
                else step (k (Chunk(BS(buffer,0,buffer.Length))))
            step i

        let enumStreamReader (reader:#System.IO.TextReader) i =
            let rec step i =
                match i with
                | Done(_,_) -> i
                | Continue k ->
                    let line = reader.ReadLine()
                    if line = null then i
                    else step (k (Chunk(ByteString.ofString line)))
            step i
