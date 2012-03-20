namespace FSharpx
#nowarn "40"

open System
open System.Collections
open System.Collections.Generic

module Monoid =
    
    /// Monoid (associative binary operation with identity)
    /// Instances
    type Dual<'a> = Dual of 'a with
        static member inline (?<-) (_     , _Monoid:Mempty , _:Dual<_>) = Dual (mempty()   )
        static member inline (?<-) (Dual x, _Monoid:Mappend,   Dual y ) = Dual (mappend y x)
    let getDual (Dual x) = x

    type Endo<'a> = Endo of ('a -> 'a) with
        static member        (?<-) (_     , _Monoid:Mempty , _:Endo<_>) = Endo id
        static member        (?<-) (Endo f, _Monoid:Mappend,   Endo g ) = Endo (f << g)

    let appEndo (Endo f) = f

    type All = All of bool with
        static member (?<-) (_    , _Monoid:Mempty , _:All  ) = All true
        static member (?<-) (All x, _Monoid:Mappend,   All y) = All (x && y)

    type Any = Any of bool with
        static member (?<-) (_    , _Monoid:Mempty , _:Any  ) = Any false
        static member (?<-) (Any x, _Monoid:Mappend,   Any y) = Any (x || y)

    type Sum<'a> = Sum of 'a with
        static member inline (?<-) (_    , _Monoid:Mempty , _:Sum<_>) = Sum LanguagePrimitives.GenericZero
        static member inline (?<-) (Sum x, _Monoid:Mappend,   Sum y ) = Sum (x + y)

    type Product<'a> = Product of 'a with
        static member inline (?<-) (_        , _Monoid:Mempty , _:Product<_>) = Product LanguagePrimitives.GenericOne
        static member inline (?<-) (Product x, _Monoid:Mappend,   Product y ) = Product (x * y)

module Async =        
    let inline bind f m = async.Bind(m,f)
    let ap  m f : Async<_> = f <*> m
    let map f m : Async<_> = fmap f m

module ZipList =
    type ZipList<'a> = ZipList of 'a seq with
        static member (?<-) (_        , _Functor    :Fmap,   ZipList x  ) = fun f      -> ZipList (Seq.map f x)
        static member (?<-) (_        , _Applicative:Pure, _:ZipList<'b>) = fun (x:'b) -> ZipList (Seq.initInfinite (konst x))
        static member (?<-) (ZipList f, _Applicative:Ap  ,   ZipList x  ) = ZipList (Seq.zip f x |> Seq.map (fun (f,x) -> f x)) 

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

    let fromNullable (n: _ Nullable) = 
        if n.HasValue
            then Some n.Value
            else None
    let toNullable =
        function
        | None -> Nullable()
        | Some x -> Nullable(x)

    let inline fromBool b = if b then Some() else None

    let inline tryParseWith func = func >> function
       | true, value -> Some value
       | false, _ -> None

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
    let create x = Nullable x
    let getOrDefault n v = match n with Value x -> x | _ -> v
    let getOrElse (n: 'a Nullable) (v: 'a Lazy) = match n with Value x -> x | _ -> v.Force()
    let get (x: _ Nullable) = x.Value
    let fromOption = Option.toNullable
    let toOption = Option.fromNullable
    let bind f x : Nullable<_> = x >>= f
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
    let map f x : Nullable<_> = fmap f x
    let toArray x = 
        match x with
        | Null -> [||]
        | Value v -> [| v |]
    let toList x =
        match x with
        | Null -> []
        | Value v -> [v]
        
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

    type State<'s,'a> = State of ('s->('a * 's)) with
        static member (?<-) (_      , _Functor:Fmap  ,   State m   ) = fun f -> State(fun s -> let (a, s') = m s in (f a, s'))

    let runState (State x) = x
    type State<'s,'a> with
        static member (?<-) (_           , _Monad  :Return  , _:State<'s,'a>) = fun a -> State(fun s -> (a, s)) : State<'s,'a>
        static member (?<-) (State m     , _Monad  :Bind    , _:State<'s,'b>) = fun k -> State(fun (s:'s) -> let (a, s') = m s in runState(k a) s') :State<'s,'b>
        static member (?<-) (_           , _Applicative:Pure, _:State<'s,'a>) = fun x -> Pure.Pure.Base x : State<'s,'a>
        static member (?<-) (f:State<_,_>, _Applicative:Ap  , x:State<'s,_> ) = Ap.Ap.Base f x

    let mapState  f (State m)  = State(f << m)
    let withState f (State m)  = State(m << f)
    let evalState (State sa) s = fst(sa s)
    let execState (State sa) s = snd(sa s)
    let get   = State (fun s -> (s , s))
    let put x = State (fun _ -> ((), x))

    type StateBuilder() =
        member this.Return(x) : State<'s,'a> = return' x
        member this.Bind(p: State<'s,'a>,rest:'a -> State<'s,'b>) = p >>= rest
        member this.Let (p,rest) = rest p
        member this.ReturnFrom(expr) = expr

        member this.Zero() = this.Return()
        member this.Combine(r1:State<_,_>, r2) = r1 >>= fun () -> r2
        member this.TryWith(m:State<'s,'a>, h:exn -> State<'s,'a>) : State<'s,'a> =
            State(fun env -> try (runState m) env
                             with e -> (runState(h e)) env)

        member this.TryFinally(m:State<'s,'a>, compensation) : State<'s,'a> =
            State(fun env -> try (runState m) env
                             finally compensation())
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                m >>= (fun () -> this.While(guard, m))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

    let state = new StateBuilder()


module Reader =
    
    type Reader<'r,'a> = Reader of ('r->'a) with
        static member (?<-) (_       , _Functor:Fmap  ,   Reader m   ) = fun f -> Reader(fun r -> f (m r))

    let runReader (Reader x) = x
    type Reader<'s,'a> with
        static member (?<-) (_            , _Monad  :Return  , _:Reader<'s,'a>) = fun a -> Reader(fun _ -> a) :Reader<'s,'a>
        static member (?<-) (Reader m     , _Monad  :Bind    , _:Reader<'s,'b>  ) = fun k -> Reader(fun r -> runReader(k (m r)) r) :Reader<'s,'b>
        static member (?<-) (_            , _Applicative:Pure, _:Reader<'s,'a>) = fun x -> Pure.Pure.Base x : Reader<'s,'a>
        static member (?<-) (f:Reader<_,_>, _Applicative:Ap  , x:Reader<'s,_> ) = Ap.Ap.Base f x

    let mapReader  f (Reader m) = Reader(f << m)
    let withReader f (Reader m) = Reader(m << f)
    let ask                = Reader id
    let local f (Reader m) = Reader(m << f)
    let asks f = do' {
        let! r = ask
        return (f r) }
    let bind k m :Reader<_,_> = k >>= m

    type ReaderBuilder() =
        member this.Return(x) :Reader<'r,'a> = return' x
        member this.Bind(p:Reader<'r,'a>,rest:'a->Reader<'r,'b>) = p >>= rest
        member this.Let (p,rest) = rest p
        member this.ReturnFrom(expr) = expr

        member this.Zero() = this.Return()
        member this.Combine(r1:Reader<_,_>, r2) = r1 >>= fun () -> r2
        member this.TryWith(m:Reader<'r,'a>, h:exn -> Reader<'r,'a>) : Reader<'r,'a> =
            Reader(fun env -> try (runReader m) env
                              with e -> (runReader(h e)) env)

        member this.TryFinally(m:Reader<'r,'a>, compensation) : Reader<'r,'a> =
            Reader(fun env -> try (runReader m) env
                              finally compensation())
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                m >>= (fun () -> this.While(guard, m))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

    let reader = new ReaderBuilder()

    
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
    
    let getHistory = get
    
    let putToHistory x = undoable {
        let! history = get
        do! put  { Current = x; 
                   Undos = history.Current :: history.Undos
                   Redos = [] } }

    let exec (State m) s = m s |> snd |> current
    
    let getCurrent<'a> = undoable {
        let! (history:'a History) = get
        return current history}

    let combineWithCurrent f x = undoable {
        let! currentVal = getCurrent
        do! putToHistory (f currentVal x) }
    
    let undo<'a> = undoable {
        let! (history:'a History) = get
        match history.Undos with
        | [] -> return false
        | (x::rest) -> 
            do! put { Current = x;
                      Undos = rest;
                      Redos = history.Current :: history.Redos }
            return true}
    
    let redo<'a> = undoable {
        let! (history:'a History) = get
        match history.Redos with
        | [] -> return false
        | (x::rest) -> 
            do! put { Current = x;
                      Undos = history.Current :: history.Undos;
                      Redos = rest }
            return true }

module Writer =
    
    type Writer<'w,'a> = Writer of ('a * 'w) with
        static member        (?<-) (_           , _Functor:Fmap  ,   Writer(a,w)) = fun f -> Writer(f a, w)

    let runWriter (Writer x) = x
    type Writer<'w,'a> with
        static member inline (?<-) (_            , _Monad:Return    , _:Writer<'s,'a>) = fun a -> Writer(a, mempty()) :Writer<'s,'a>
        static member inline (?<-) (Writer(a, w) , _Monad:Bind      , _:Writer<'s,'b>) = fun k -> Writer(let (b, w') = runWriter(k a) in (b, mappend w w')) :Writer<'s,'b>
        static member inline (?<-) (_            , _Applicative:Pure, _:Writer<'s,'a>) = fun a -> Writer(a, mempty()) :Writer<'s,'a>
        static member inline (?<-) (f:Writer<_,_>, _Applicative:Ap  , x:Writer<_,_>  ) = Ap.Ap.Base f x : Writer<_,_>

    let mapWriter f (Writer m)   = Writer(f m)
    let execWriter  (Writer m) s = snd m

    let tell              w       = Writer((),     w)
    let listen(Writer (a, w))     = Writer((a, w), w)
    let pass  (Writer((a, f), w)) = Writer( a,   f w)

    let inline listens f m = do' {
        let! (a, w) = listen m
        return (a, f w)}

    let inline censor f m = pass <| do' {
        let! a = m
        return (a, f)}

    type WriterBuilder() =
        member inline this.Return(x) :Writer<'w,'a> = return' x
        member inline this.Bind(p:Writer<'w,'a>,rest:'a->Writer<'w,'b>) = p >>= rest
        member this.Let (p,rest) = rest p
        member this.ReturnFrom(expr) = expr

        member inline this.Zero() = this.Return()
        member inline this.Combine(r1:Writer<_,_>, r2) = r1 >>= fun () -> r2
        member inline this.Delay(f) = this.Bind(this.Return (), f)

        member this.TryWith(m:Writer<'w,'a>, h:exn -> Writer<'w,'a>) : Writer<'w,'a> =
            Writer(try (runWriter m)
                   with e -> (runWriter(h e)))

        member this.TryFinally(m:Writer<'w,'a>, compensation) : Writer<'w,'a> =
            Writer(try (runWriter m)
                   finally compensation()) 
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        
        member inline this.While(guard, m) =
            let rec f guard m =
                if not(guard()) then this.Zero() else
                    m() >>= (fun () -> f guard m)
            f guard m
        member inline this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                fun enum -> this.While(enum.MoveNext, fun () -> body enum.Current))

    let writer = new WriterBuilder()

module Choice =

    let get =
        function
        | Choice1Of2 a -> a
        | Choice2Of2 e -> invalidArg "choice" (sprintf "The choice value was Choice2Of2 '%A'" e)

    let inline protect f x = 
        try
            Choice1Of2 (f x)
        with e -> Choice2Of2 e

    let inline cast (o: obj) = protect unbox o

    let map f x :Choice<_,_> = fmap f x

    let bind f m : Choice<_,_> = m >>= f

    let inline bimap f1 f2 = 
        function
        | Choice1Of2 x -> Choice1Of2 (f1 x)
        | Choice2Of2 x -> Choice2Of2 (f2 x)

    let inline choice f1 f2 = 
        function
        | Choice1Of2 x -> f1 x
        | Choice2Of2 x -> f2 x

    let inline mapSecond f = bimap id f

    let toOption = Option.fromChoice
    let fromOption o = 
        function
        | Some a -> Choice1Of2 a
        | None -> Choice2Of2 o
    
    let toValidation = function
                       | Choice1Of2 x -> Right x
                       | Choice2Of2 x -> Left  x

module Validation =

    let toChoice = function
                   | Right x -> Choice1Of2 x
                   | Left  x -> Choice2Of2 x    

    let (|Success|Failure|) = 
        function
        | Right a -> Success a
        | Left  e -> Failure e

    let inline seqValidator f = 
        let zero = pure' []
        Seq.map (f) >> Seq.fold (lift2 (flip FSharpx.List.cons)) zero

    let inline ap x f  :Validation<_,_> = f <*> x


module Continuation =

    /// The continuation monad.

    type Cont<'r,'a> = Cont of (('a->'r)->'r) with
        static member (?<-) (_     , _Functor:Fmap  ,   Cont m    ) = fun f -> Cont(fun c -> m (c << f))
    
    let runCont (Cont x) = x
    type Cont<'r,'a> with
        static member (?<-) (_          , _Monad  :Return  , _:Cont<'r,'a>) = fun (n:'a) -> Cont(fun k -> k n) :Cont<'r,'a>
        static member (?<-) (Cont m     , _Monad  :Bind    , _:Cont<'r,'b>) = fun  f     -> Cont(fun k -> m (fun a -> runCont(f a) k)) :Cont<'r,'b>
        static member (?<-) (_          , _Applicative:Pure, _:Cont<'r,'a>) = fun x -> Pure.Pure.Base x : Cont<'r,'a>
        static member (?<-) (f:Cont<_,_>, _Applicative:Ap  , x:Cont<'r,_> ) = Ap.Ap.Base f x

    let callCC f = Cont <| fun k -> runCont (f (fun a -> Cont(fun _ -> k a))) k

    type ContinuationBuilder() =
        member this.Return(x) :Cont<'r,'a> = return' x
        member this.Bind(p:Cont<'r,'a>,rest:'a->Cont<'r,'b>) = p >>= rest
        member this.Let (p,rest) = rest p
        member this.ReturnFrom(expr) = expr

        member this.Zero() = this.Return()
        member this.Combine(r1:Cont<_,_>, r2) = r1 >>= fun () -> r2

        member this.TryWith(m:Cont<'r,'a>, h:exn -> Cont<'r,'a>) : Cont<'r,'a> =
            Cont(fun env -> try (runCont m) env
                              with e -> (runCont(h e)) env)

        member this.TryFinally(m:Cont<'r,'a>, compensation) : Cont<'r,'a> =
            Cont(fun env -> try (runCont m) env
                            finally compensation())
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                m >>= (fun () -> this.While(guard, m))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

    let cont = ContinuationBuilder()

    /// The coroutine type from http://fssnip.net/7M
    type Coroutine() =
        let tasks = new Queue<Cont<unit,unit>>()

        member this.Put(task) =
            let withYield = cont {
                do! callCC <| fun exit ->
                    task <| fun () ->
                    callCC <| fun c ->
                    tasks.Enqueue(c())
                    exit()
                if tasks.Count <> 0 then
                    do! tasks.Dequeue() }
            tasks.Enqueue(withYield)
            
        member this.Run() =
            runCont (tasks.Dequeue()) ignore

module Distribution =
    
    type 'a Outcome = {
        Value: 'a
        Probability : BigRational    }

    type Distribution<'a> = Distribution of 'a Outcome seq with
        static member (?<-) (_     , _Functor:Fmap  ,   Distribution m    ) = fun f -> 
            Distribution(Seq.map (fun o -> { Value = f o.Value; Probability = o.Probability }) m)
    
    let runDistribution (Distribution d) = d
    type Distribution<'a> with
        static member (?<-) (_             , _Monad  :Return, _:Distribution<'a>) = fun (n:'a) ->
            Distribution(Seq.singleton { Value = n ; Probability = 1N/1N })
        static member (?<-) (Distribution m, _Monad  :Bind  , _:Distribution<_> ) = fun  f     ->
            m 
            |> Seq.map (fun p1 -> 
                f p1.Value
                |> runDistribution |> Seq.map (fun (p2) -> 
                    { Value = p2.Value; 
                        Probability = 
                            p1.Probability * p2.Probability}))
            |> Seq.concat
            |> Distribution
        static member (?<-) (_                 , _Applicative:Pure, _:Distribution<'a>) = fun (x:'a) -> Pure.Pure.Base x :Distribution<'a>
        static member (?<-) (f:Distribution<_> , _Applicative:Ap  , x:Distribution<_> ) = Ap.Ap.Base f x
    
    // P(A AND B) = P(A | B) * P(B)
    let bind (f: 'a -> 'b Distribution) (dist:'a Distribution) =
        dist >>= f : 'b Distribution
    
    type DistributionMonadBuilder() =
        member this.Bind (r, f) = bind f r
        member this.Return x : Distribution<_> = return' x
        member this.ReturnFrom x = x
    
    let distribution = DistributionMonadBuilder()
    
    // Create some helpers
    let toUniformDistribution seq : 'a Distribution =
        let l = Seq.length seq
        seq 
        |> Seq.map (fun e ->
            { Value = e; 
                Probability = 1N / bignum.FromInt l })
        |> Distribution
    
    let probability (Distribution dist:'a Distribution) = 
        dist
        |> Seq.map (fun o -> o.Probability)
        |> Seq.sum
    
    let certainly x : Distribution<_> = return' x
    let impossible<'a> :'a Distribution = toUniformDistribution []
    
    let fairDice sides = toUniformDistribution [1..sides]
    
    type CoinSide = 
        | Heads 
        | Tails
    
    let fairCoin = toUniformDistribution [Heads; Tails]
    
    let filter predicate (Distribution dist:'a Distribution) : 'a Distribution =
        dist |> Seq.filter (fun o -> predicate o.Value) |> Distribution
    
    let filterInAnyOrder items dist =
        items
        |> Seq.fold (fun d item -> filter (Seq.exists ((=) (item))) d) dist
    
    let map f (dist:'a Distribution) : 'b Distribution = 
        fmap f dist
    
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
