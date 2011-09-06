namespace FSharpx
#nowarn "40"

open System

module Monoid =
  open System.Collections.Generic
  
  /// The monoid.
  /// The monoid implementation comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx.
  type IMonoid<'a> =
    abstract member mempty  : unit -> 'a
    abstract member mappend : 'a * 'a -> 'a
  
  type MonoidAssociations private() =
    static let associations = new Dictionary<Type, obj>()
    static member Add<'a>(monoid : IMonoid<'a>) = associations.Add(typeof<'a>, monoid)
    static member Get<'a>() =
      match associations.TryGetValue(typeof<'a>) with
      | true, assoc -> assoc :?> IMonoid<'a>
      | false, _    -> failwithf "No IMonoid defined for %O" <| typeof<'a>
  
  let mempty<'a> = MonoidAssociations.Get<'a>().mempty
  let mappend<'a> a b = MonoidAssociations.Get<'a>().mappend(a, b)
  
module Operators =

  let inline returnM builder x = (^M: (member Return: 'b -> 'c) (builder, x))
  let inline bindM builder m f = (^M: (member Bind: 'd -> ('e -> 'c) -> 'c) (builder, m, f))
  let inline liftM builder f m =
    let inline ret x = returnM builder (f x)
    bindM builder m ret
  let inline applyM (builder1:^M1) (builder2:^M2) f m =
    bindM builder1 f <| fun f' ->
      bindM builder2 m <| fun m' ->
        returnM builder2 (f' m') 

module Async =
  open Operators
    
  let inline returnM x = returnM async x
  let inline (>>=) m f = bindM async m f
  let inline (<*>) f m = applyM async async f m
  let inline pipe m f = liftM async f m
  let inline pipe2 x y f = returnM f <*> x <*> y
  let inline pipe3 x y z f = returnM f <*> x <*> y <*> z
  let inline (<!>) f m = pipe m f
  let inline ( *>) x y = pipe2 x y (fun _ z -> z)
  let inline ( <*) x y = pipe2 x y (fun z _ -> z)
  let inline (>>.) m f = bindM async m (fun _ -> f)

module Maybe =

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
  let inline (<*>) f m = applyM maybe maybe f m
  let inline lift f m = liftM maybe f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM maybe m (fun _ -> f)

module State =

  type State<'a, 's> = 's -> 'a * 's
  
  let getState = fun s -> (s,s)
  let putState s = fun _ -> ((),s)
  let eval m s = m s |> fst
  let exec m s = m s |> snd
  let empty = fun s -> ((), s)
  
  /// The state monad.
  /// The algorithm is adjusted from my original work off of Brian Beckman's http://channel9.msdn.com/shows/Going+Deep/Brian-Beckman-The-Zen-of-Expressing-State-The-State-Monad/.
  /// The approach was adjusted from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2009/12/30/much-ado-about-monads-state-edition.aspx and mirrors his final result.
  type StateBuilder() =
    member this.Return(a) : State<'a,'s> = fun s -> (a,s)
    member this.ReturnFrom(m:State<'a,'s>) = m
    member this.Bind(m:State<'a,'s>, k:'a -> State<'b,'s>) : State<'b,'s> =
      fun s -> let (a, s') = m s in (k a) s'
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
  let inline (<*>) f m = applyM state state f m
  let inline lift f m = liftM state f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM state m (fun _ -> f)

module Reader =

  type Reader<'r,'a> = 'r -> 'a
  
  /// The reader monad.
  /// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/07/much-ado-about-monads-reader-edition.aspx.
  type ReaderBuilder() =
    member this.Return(a) : Reader<'r,'a> = fun _ -> a
    member this.ReturnFrom(a:Reader<'r,'a>) = a
    member this.Bind(m:Reader<'r,'a>, k:'a -> Reader<'r,'b>) : Reader<'r,'b> =
      fun r -> (k (m r)) r
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
  let inline (<*>) f m = applyM reader reader f m
  let inline lift f m = liftM reader f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM reader m (fun _ -> f)

module Undo =
  // UndoMonad on top of StateMonad
  open State
  
  let undoable = state
  
  type 'a History =  { 
      Current:'a
      Undos : 'a list
      Redos : 'a list }
  
  let empty x = { Current = x; Undos = []; Redos = [] }
  let current history = history.Current
  
  let getHistory = getState
  
  let putToHistory x = undoable {
      let! history = getState
      do! putState 
           { Current = x; 
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
          do! putState 
                  { Current = x;
                    Undos = rest;
                    Redos = history.Current :: history.Redos }
          return true}
  
  
  let redo<'a> = undoable {
      let! (history:'a History) = getState
      match history.Redos with
      | [] -> return false
      | (x::rest) -> 
          do! putState 
                  { Current = x;
                    Undos = history.Current :: history.Undos;
                    Redos = rest }
          return true}

module Writer =
  open Monoid
  
  type ListMonoid<'a>() =
    interface IMonoid<'a list> with
      member this.mempty() = []
      member this.mappend(a,b) = a @ b
  
  MonoidAssociations.Add(new ListMonoid<string>())
  
  type Writer<'w, 'a> = unit -> 'a * 'w
  
  /// The writer monad.
  /// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx.
  type WriterBuilder() =
    member this.Return(a) : Writer<'w,'a> = fun () -> (a, mempty())
    member this.ReturnFrom(w:Writer<'w,'a>) = w
    member this.Bind(writer:Writer<'w,'a>, k:'a -> Writer<'w,'b>) : Writer<'w,'b> =
      fun () ->
        let (a, w) = writer()
        let (a', w') = (k a)()
        (a', mappend w w')
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
      | _    -> this.Zero()
    member this.For(sequence:seq<'a>, body:'a -> Writer<'w,unit>) =
      this.Using(sequence.GetEnumerator(), 
                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
  let writer = new WriterBuilder()
  
  let tell   w = fun () -> ((), w)
  let listen m = fun () -> let (a, w) = m() in ((a, w), w)
  let pass   m = fun () -> let ((a, f), w) = m() in (a, f w)
  
  let listens f m = writer {
    let! (a, b) = m
    return (a, f b) }
  
  let censor (f:'w1 -> 'w2) (m:Writer<'w1,'a>) : Writer<'w2,'a> =
    writer { let! a = m
             return (a, f)
           } |> pass
  
  open Operators
  
  let inline returnM x = returnM writer x
  let inline (>>=) m f = bindM writer m f
  let inline (<*>) f m = applyM writer writer f m
  let inline lift f m = liftM writer f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM writer m (fun _ -> f)

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
  let callCC (f: ('a -> Cont<'b,'r>) -> Cont<'a,'r>) : Cont<'a,'r> =
    fun cont econt -> runCont (f (fun a -> (fun _ _ -> cont a))) cont econt
   
  type ContinuationBuilder() =
    member this.Return(a) : Cont<_,_> = fun cont econt -> cont a
    member this.ReturnFrom(comp:Cont<_,_>) = comp
    member this.Bind(comp1, f) = fun cont econt ->
      runCont comp1 (fun a -> protect f a (fun comp2 -> runCont comp2 cont econt) econt) econt
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
      this.Using(items.GetEnumerator(), (fun enum -> this.While((fun () -> enum.MoveNext()), this.Delay(fun () -> body enum.Current))))
  let cont = ContinuationBuilder()
  
  open Operators
  
  let inline returnM x = returnM cont x
  let inline (>>=) m f = bindM cont m f
  let inline (<*>) f m = applyM cont cont f m
  let inline lift f m = liftM cont f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM cont m (fun _ -> f)

  /// The coroutine type from http://fssnip.net/7M
  type Coroutine() =
    let tasks = new System.Collections.Generic.Queue<Cont<unit,unit>>()

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
      runCont (tasks.Dequeue()) ignore raise

module Distribution =
  
  type 'a Outcome = {
      Value: 'a
      Probability : BigRational  }
  
  type 'a Distribution = 'a Outcome seq
  
  // P(A AND B) = P(A | B) * P(B)
  let bindD (dist:'a Distribution) (f: 'a -> 'b Distribution) =
      dist 
          |> Seq.map (fun p1 -> 
                  f p1.Value
                  |> Seq.map (fun p2 -> 
                          { Value = p2.Value; 
                            Probability = 
                              p1.Probability * p2.Probability}))
          |> Seq.concat : 'b Distribution
  
  let inline (>>=) dist f = bindD dist f
  
  let returnD (value:'a) =   
      Seq.singleton { Value = value ; Probability = 1N/1N }
        : 'a Distribution
  
  type DistributionMonadBuilder() =
      member this.Bind (r, f) = bindD r f
      member this.Return x = returnD x
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
  
  let certainly = returnD
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

module Iteratee =
  open FSharpx
  
  /// A stream of chunks of data generated by an Enumerator.
  /// The stream can be composed of chunks of 'a, empty blocks indicating a wait, or an EOF marker.
  /// In Haskell, the Chunk is usually composed of a list of ListLike type, but F# doesn't support
  /// Monad Transforms or ^M in type declarations. Thus, the Chunk is left open to various internal
  /// types, but a bit more work must be done in order to maintain the meaningfulness of "chunk".
  /// That said, the 'a allows a large number of chunk-y types to be used, including other monads.
  /// Be aware that when using #seq<_> types, you will need to check for both Seq.empty ([]) and Empty.
  type Stream<'a> =
    | Chunk of 'a
    | Empty
    | EOF
  
  // TODO: Stream Monoid? I had this when using Chunk of 'a list, but trying to create a generic monoid appears impossible. I'll have to add multiple versions of everything in order to support such a construct. The Empty discriminator provides much of what was needed.
  
  /// The iteratee is a stream consumer that will consume a stream of data until either 
  /// it receives an EOF or meets its own requirements for consuming data. The iteratee
  /// will return Continue whenever it is ready to receive the next chunk. An iteratee
  /// is fed data by an Enumerator, which generates a Stream. 
  type Iteratee<'el,'a> =
    | Yield of 'a * Stream<'el>
    | Error of exn
    | Continue of (Stream<'el> -> Iteratee<'el,'a>)
  
  /// An enumerator generates a stream of data and feeds an iteratee, returning a new iteratee.
  type Enumerator<'el,'a> = Iteratee<'el,'a> -> Iteratee<'el,'a>
  
  /// An Enumeratee is an Enumerator that feeds data streams to an internal iteratee.
  type Enumeratee<'elo,'eli,'a> = Iteratee<'eli,'a> -> Iteratee<'elo, Iteratee<'eli,'a>>
  
  [<AutoOpen>]
  module Primitives =
  
    let yieldI x s = Yield(x,s)
    let continueI k = Continue k
  
    let bind m f =
      let rec innerBind = function
        | Continue k -> Continue(innerBind << k)
        | Error e -> Error e
        | Yield(x, Empty) -> f x
        | Yield(x, extra) ->
            match f x with
            | Continue k -> k extra
            | Error e -> Error e
            | Yield(acc',_) -> Yield(acc', extra)
      innerBind m
  
    let throw e = Error e
  
    let catchError h i =
      let rec step = function 
        | Error e -> h e
        | Continue k -> continueI (fun s -> step (k s))
        | i -> i
      in step i
  
    let tryFinally compensation i =
      let rec step = function 
        | Continue k -> continueI (fun s -> step (k s))
        | i -> compensation(); i
      in step i
  
    let rec enumEOF = function 
      | Yield(x,_) -> Yield(x, EOF)
      | Error e -> throw e
      | Continue k ->
          match k EOF with
          | Continue _ -> failwith "enumEOF: divergent iteratee"
          | i -> enumEOF i
    
    let enumErr e = function _ -> Error e
  
    let run i =
      match enumEOF i with
      | Error e -> Choice1Of2 e
      | Yield(x,_) -> Choice2Of2 x
      | Continue _ -> failwith "run: divergent iteratee"
    
    let run_ i =
      match run i with
      | Choice1Of2 e -> raise e
      | x -> x
    
  type IterateeBuilder() =
    member this.Return(x) = Yield(x, Empty)
    member this.ReturnFrom(m:Iteratee<_,_>) = m
    member this.Bind(m, k) = bind m k
    member this.Zero() = Yield((), Empty)
    member this.Combine(comp1, comp2) = bind comp1 <| fun () -> comp2
    member this.Delay(f) = bind (Yield((), Empty)) f
    member this.TryWith(m, h) = catchError h m
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
  
  let inline returnM x = Yield(x, Empty)
  let inline (>>=) m f = bind m f
  let inline (<*>) f m = f >>= fun f' -> m >>= fun m' -> returnM (f' m')
  let inline lift f m = m >>= fun x -> returnM (f x)
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = m >>= (fun _ -> f)
  
  module List =
    open Operators
    
    let fold step seed =
      let f = List.fold step
      let rec loop acc = function
        | Empty -> Continue (loop acc)
        | Chunk [] -> Continue (loop acc)
        | Chunk xs -> Continue (loop (f acc xs))
        | EOF -> Yield(acc, EOF)
      Continue (loop seed)
    
    let length<'a> : Iteratee<'a list, int> =
      let rec step n = function
        | Empty | Chunk [] -> Continue (step n)
        | Chunk x          -> Continue (step (n + x.Length))
        | EOF         as s -> Yield(n, s)
      in Continue (step 0)
    
    let peek<'a> : Iteratee<'a list, 'a option> =
      let rec inner =
        let rec step = function
          | Empty | Chunk ([]:'a list) -> inner
          | Chunk(x::xs) as s -> Yield(Some x, s)
          | s -> Yield(None, s)
        Continue step
      in inner
    
    let head<'a> : Iteratee<'a list, 'a option> =
      let rec inner =
        let rec step = function
          | Empty | Chunk ([]:'a list) -> inner
          | Chunk(x::xs) -> Yield(Some x, Chunk xs)
          | EOF -> Yield(None, EOF)
        Continue step
      in inner
    
    let drop n =
      let rec step n = function
        | Empty | Chunk [] -> Continue <| step n
        | Chunk str ->
            if str.Length < n then
              Continue <| step (n - str.Length)
            else let extra = List.skip n str in Yield((), Chunk extra)
        | EOF -> Yield((), EOF)
      in if n <= 0 then Yield((), Empty) else Continue (step n)
    
    let private dropWithPredicate pred listOp =
      let rec step = function
        | Empty | Chunk [] -> Continue step
        | Chunk x ->
            match listOp pred x with
            | [] -> Continue step
            | x' -> Yield((), Chunk x')
        | EOF as s -> Yield((), s)
      in Continue step

    let dropWhile pred = dropWithPredicate pred List.skipWhile
    let dropUntil pred = dropWithPredicate pred List.skipUntil
    
    let take n =
      let rec step before n = function
        | Empty | Chunk [] -> Continue <| step before n
        | Chunk str ->
            if str.Length < n then
              Continue <| step (before @ str) (n - str.Length)
            else let str', extra = List.splitAt n str in Yield(before @ str', Chunk extra)
        | EOF -> Yield(before, EOF)
      in if n <= 0 then Yield([], Empty) else Continue (step [] n)
    
    let private takeWithPredicate (pred:'a -> bool) listOp =
      let rec step before = function
        | Empty | Chunk [] -> Continue (step before)
        | Chunk str ->
            match listOp pred str with
            | str', [] -> Continue (step (before @ str'))
            | str', extra -> Yield(before @ str', Chunk extra)
        | EOF -> Yield(before, EOF)
      in Continue (step [])
    
    let takeWhile pred = takeWithPredicate pred List.span
    let takeUntil pred = takeWithPredicate pred List.split
    
    let heads str =
      let rec loop count str =
        match count, str with
        | (count, []) -> Yield(count, EOF)
        | (count, str) -> Continue (step count str)
      and step count str s =
        match str, s with
        | str, Empty -> loop count str
        | str, (Chunk []) -> loop count str
        | c::t, (Chunk (c'::t')) ->
            if c = c' then step (count + 1) t (Chunk t') 
            else Yield(count, Chunk (c'::t'))
        | _, s -> Yield(count, s)
      loop 0 str
    
    let readLines =
      let toString chars = String(Array.ofList chars)
      let newlines = ['\r';'\n']
      let newline = ['\n']
      let isNewline c = c = '\r' || c = '\n'
      let terminators = heads newlines >>= fun n -> if n = 0 then heads newline else Yield(n, Empty)
      let rec lines acc = takeUntil isNewline >>= fun l -> terminators >>= check acc l
      and check acc l count =
        match l, count with
        | _, 0 -> Yield(Choice1Of2 (List.rev acc |> List.map toString), Chunk l)
        | [], _ -> Yield(Choice2Of2 (List.rev acc |> List.map toString), EOF)
        | l, _ -> lines (l::acc)
      lines []
    
    (* ========= Enumerators ========= *)
    
    //val enumerate :: 'a list -> Enumerator<'a list,'b>
    let rec enumerate str i = 
      match str, i with
      | [], Continue k -> Continue k
      | x::xs, Continue k -> enumerate xs (k (Chunk [x]))
      | _ -> i
    
    // val enumeratePure1Chunk :: 'a list -> Enumerator<'a list,'b>
    let enumeratePure1Chunk (str:'a list) = function
      | Continue k -> k (Chunk str)
      | i -> i
    
    // val enumeratePureNChunk :: 'a list -> int -> Enumerator<'a list,'b>
    let rec enumeratePureNChunk n str i =
      match str, i with
      | _::_, Continue k ->
          let s1, s2 = List.splitAt n str
          enumeratePureNChunk n s2 (k (Chunk s1))
      | _ -> i
  
  module Binary =
    open Operators
    
    (* ========= Iteratees ========= *)
    
    let fold step seed =
      let f = ByteString.fold step
      let rec loop acc = function
        | Empty -> Continue (loop acc)
        | Chunk xs when ByteString.isEmpty xs -> Continue (loop acc)
        | Chunk xs -> Continue (loop (f acc xs))
        | EOF -> Yield(acc, EOF)
      Continue (loop seed)
    
    let length = 
      let rec step n = function
        | Empty -> Continue (step n)
        | Chunk x when ByteString.isEmpty x -> Continue (step n)
        | Chunk x -> Continue (step (n + x.Count))
        | EOF as s -> Yield(n, s)
      Continue (step 0)
    
    let rec peek =
      let rec step = function
        | Empty -> peek
        | Chunk x when ByteString.isEmpty x -> peek
        | Chunk x as s -> Yield(Some(ByteString.head x), s)
        | s -> Yield(None, s)
      Continue step
    
    let rec head =
      let rec step = function
        | Empty -> head 
        | Chunk x when ByteString.isEmpty x -> head
        | Chunk x -> Yield(Some(ByteString.head x), Chunk(ByteString.tail x))
        | s -> Yield(None, s)
      Continue step

    let drop n =
      let rec step n = function
        | Empty -> Continue <| step n
        | Chunk str when ByteString.isEmpty str -> Continue <| step n
        | Chunk str ->
            if ByteString.length str < n then
              Continue <| step (n - (ByteString.length str))
            else let extra = ByteString.skip n str in Yield((), Chunk extra)
        | EOF -> Yield((), EOF)
      in if n <= 0 then Yield((), Empty) else Continue (step n)
    
    let private dropWithPredicate pred byteStringOp =
      let rec step = function
        | Empty -> Continue step
        | Chunk x when ByteString.isEmpty x -> Continue step
        | Chunk x ->
            let x' = byteStringOp pred x
            in if ByteString.isEmpty x' then Continue step
               else Yield((), Chunk x')
        | s -> Yield((), s)
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
            else let str', extra = ByteString.splitAt n str in Yield(ByteString.append before str', Chunk extra)
        | EOF -> Yield(before, EOF)
      in if n <= 0 then Yield(ByteString.empty, Empty) else Continue (step ByteString.empty n)
    
    let private takeWithPredicate (pred:'a -> bool) byteStringOp =
      let rec step before = function
        | Empty -> Continue <| step before
        | Chunk str when ByteString.isEmpty str -> Continue <| step before
        | Chunk str ->
            match byteStringOp pred str with
            | str', extra when ByteString.isEmpty extra -> Continue <| step (ByteString.append before str')
            | str', extra -> Yield(ByteString.append before str', Chunk extra)
        | EOF -> Yield(before, EOF)
      Continue (step ByteString.empty)
    
    let takeWhile pred = takeWithPredicate pred ByteString.span
    let takeUntil pred = takeWithPredicate pred ByteString.split
    
    let heads str =
      let rec loop count str =
        if ByteString.isEmpty str then Yield(count, EOF)
        else Continue (step count str)
      and step count str = function
        | Empty -> loop count str
        | Chunk x when ByteString.isEmpty x -> loop count str
        | Chunk x when not (ByteString.isEmpty str) ->
            let c, t = ByteString.head str, ByteString.tail str
            let c', t' = ByteString.head x, ByteString.tail x
            if c = c' then step (count + 1) t (Chunk t') 
            else Yield(count, Chunk x)
        | s -> Yield(count, s)
      loop 0 str
    
    let readLines =
      let crlf = ByteString.create "\r\n"B
      let lf = ByteString.singleton '\n'B
      let isNewline c = c = '\r'B || c = '\n'B
      let terminators = heads crlf >>= fun n -> if n = 0 then heads lf else Yield(n, Empty)
      let rec lines acc = takeUntil isNewline >>= fun bs -> terminators >>= check acc bs
      and check acc bs count =
        if count = 0 then
          Yield(Choice1Of2 (List.rev acc |> List.map ByteString.toString), Chunk bs)
        elif ByteString.isEmpty bs then
          Yield(Choice2Of2 (List.rev acc |> List.map ByteString.toString), EOF)
        else lines (bs::acc)
      lines []
    
    (* ========= Enumerators ========= *)
    
    // val enumerate :: ByteString -> Enumerator<ByteString,'b>
    let rec enumerate str = function
      | Continue k when not (ByteString.isEmpty str) ->
          let x, xs = ByteString.head str, ByteString.tail str
          enumerate xs (k (Chunk (ByteString.singleton x)))
      | i -> i
    
    // val enumeratePure1Chunk :: ByteString -> Enumerator<ByteString,'b>
    let enumeratePure1Chunk str = function
      | Continue k -> k (Chunk str)
      | i -> i
    
    // val enumeratePureNChunk :: ByteString -> int -> Enumerator<ByteString,'b>
    let rec enumeratePureNChunk n str = function
      | Continue k when not (ByteString.isEmpty str) ->
          let s1, s2 = ByteString.splitAt n str
          enumeratePureNChunk n s2 (k (Chunk s1))
      | i -> i
    
    let enumStream bufferSize (stream:#System.IO.Stream) i =
      let buffer = Array.zeroCreate<byte> bufferSize
      let rec step = function Continue k -> read k | i -> i
      and read k =
        let result =
          try Choice2Of2(stream.Read(buffer, 0, bufferSize))
          with e -> Choice1Of2 e
        match result with
        | Choice1Of2 e -> throw e
        | Choice2Of2 0 -> Continue k
        | Choice2Of2 n -> step (k (Chunk(ByteString.BS(buffer,0,buffer.Length))))
      step i
  