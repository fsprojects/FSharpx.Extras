// bootstrapped queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/18/bootstrapped-queue
module FSharpx.DataStructures.BootstrappedQueue

open FSharpx

type NonEmptyBootstrappedQueue<'a> = {
    FrontAndSuspensionsLength : int
    Front : list<'a>
    Suspensions : BootstrappedQueue<Lazy<list<'a>>>
    RBackLength : int
    RBack : list<'a>
} with
    static member create lenfm f m lenr r = {
        FrontAndSuspensionsLength = lenfm
        Front = f
        Suspensions = m
        RBackLength = lenr
        RBack = r
    }  

and BootstrappedQueue<'a> = Empty | NonEmpty of NonEmptyBootstrappedQueue<'a> with
    // polymorphic recursion cannot be achieved through let-bound functions
    // hence we use static member methods 

    static member checkQ (q:NonEmptyBootstrappedQueue<'a>) =
        if q.RBackLength <= q.FrontAndSuspensionsLength then BootstrappedQueue.checkF q else
        let susp = BootstrappedQueue.snoc (lazy List.rev q.RBack) q.Suspensions
        NonEmptyBootstrappedQueue<'a>.create (q.FrontAndSuspensionsLength + q.RBackLength) q.Front susp 0 []
        |> BootstrappedQueue.checkF 

    static member checkF (q:NonEmptyBootstrappedQueue<'a>) : BootstrappedQueue<'a> =
        match q.Front, q.Suspensions with
        | [], Empty -> Empty
        | [], m ->
            let f = BootstrappedQueue.head m |> Lazy.force
            let susp = BootstrappedQueue.tail m
            NonEmpty <| NonEmptyBootstrappedQueue<'a>.create q.FrontAndSuspensionsLength f susp q.RBackLength q.RBack
        | _ -> NonEmpty q

    static member snoc (x:'a) : BootstrappedQueue<'a> -> BootstrappedQueue<'a> = function
        | Empty -> NonEmpty <| NonEmptyBootstrappedQueue<'a>.create 1 [x] Empty 0 []
        | NonEmpty q ->
            let lenr = q.RBackLength+1
            let r = x::q.RBack
            NonEmptyBootstrappedQueue<'a>.create q.FrontAndSuspensionsLength q.Front q.Suspensions lenr r
            |> BootstrappedQueue<'a>.checkQ 

    static member head : BootstrappedQueue<'a> -> 'a = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q -> List.head q.Front

    static member tryGetHead : BootstrappedQueue<'a> -> 'a option = function
        | Empty -> None
        | NonEmpty q -> Some (List.head q.Front)

    static member tail : BootstrappedQueue<'a> -> BootstrappedQueue<'a> = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q ->
            let lenfm = q.FrontAndSuspensionsLength - 1
            let f' = List.tail q.Front
            NonEmptyBootstrappedQueue<'a>.create lenfm f' q.Suspensions q.RBackLength q.RBack
            |> BootstrappedQueue<'a>.checkQ

    static member tryGetTail : BootstrappedQueue<'a> -> BootstrappedQueue<'a> option = function
        | Empty -> None
        | NonEmpty q ->
            let lenfm = q.FrontAndSuspensionsLength - 1
            let f' = List.tail q.Front
            NonEmptyBootstrappedQueue<'a>.create lenfm f' q.Suspensions q.RBackLength q.RBack
            |> BootstrappedQueue<'a>.checkQ
            |> Some

    static member length : BootstrappedQueue<'a> -> int = function
        | Empty -> 0
        | NonEmpty q -> q.FrontAndSuspensionsLength + q.RBackLength

    static member ofList (l:List<'a>) : BootstrappedQueue<'a> = 
        let b0 = BootstrappedQueue.Empty
        NonEmptyBootstrappedQueue<'a>.create (l.Length) l b0 0 [] |> NonEmpty

let empty = Empty
let isEmpty = function Empty -> true | _ -> false

let inline snoc x queue = BootstrappedQueue.snoc x queue
let inline head queue = BootstrappedQueue<'a>.head queue
let inline tryGetHead queue = BootstrappedQueue<'a>.tryGetHead queue
let inline tail queue = BootstrappedQueue<'a>.tail queue
let inline tryGetTail queue = BootstrappedQueue<'a>.tryGetTail queue
let inline length queue = BootstrappedQueue<'a>.length queue
let inline ofList list = BootstrappedQueue<'a>.ofList list