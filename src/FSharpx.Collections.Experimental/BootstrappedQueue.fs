// bootstrapped queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/18/bootstrapped-queue
module FSharpx.Collections.Experimental.BootstrappedQueue

open FSharpx

type NonEmptyBootstrappedQueue<'T> = {
    FrontAndSuspensionsLength : int
    Front : list<'T>
    Suspensions : BootstrappedQueue<Lazy<list<'T>>>
    RBackLength : int
    RBack : list<'T>
} with
    static member create lenfm f m lenr r = {
        FrontAndSuspensionsLength = lenfm
        Front = f
        Suspensions = m
        RBackLength = lenr
        RBack = r
    }  

and BootstrappedQueue<'T> = 
    | Empty 
    | NonEmpty of NonEmptyBootstrappedQueue<'T> 

    // polymorphic recursion cannot be achieved through let-bound functions
    // hence we use static member methods 

    static member checkQ (q:NonEmptyBootstrappedQueue<'T>) =
        if q.RBackLength <= q.FrontAndSuspensionsLength then BootstrappedQueue.checkF q else
        let susp = BootstrappedQueue.snoc (lazy List.rev q.RBack) q.Suspensions
        NonEmptyBootstrappedQueue<'T>.create (q.FrontAndSuspensionsLength + q.RBackLength) q.Front susp 0 []
        |> BootstrappedQueue.checkF 

    static member checkF (q:NonEmptyBootstrappedQueue<'T>) : BootstrappedQueue<'T> =
        match q.Front, q.Suspensions with
        | [], Empty -> Empty
        | [], m ->
            let f = BootstrappedQueue.head m |> Lazy.force
            let susp = BootstrappedQueue.tail m
            NonEmpty <| NonEmptyBootstrappedQueue<'T>.create q.FrontAndSuspensionsLength f susp q.RBackLength q.RBack
        | _ -> NonEmpty q

    static member snoc (x:'T) : BootstrappedQueue<'T> -> BootstrappedQueue<'T> = function
        | Empty -> NonEmpty <| NonEmptyBootstrappedQueue<'T>.create 1 [x] Empty 0 []
        | NonEmpty q ->
            let lenr = q.RBackLength+1
            let r = x::q.RBack
            NonEmptyBootstrappedQueue<'T>.create q.FrontAndSuspensionsLength q.Front q.Suspensions lenr r
            |> BootstrappedQueue<'T>.checkQ 

    static member head : BootstrappedQueue<'T> -> 'T = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q -> List.head q.Front

    static member tryGetHead : BootstrappedQueue<'T> -> 'T option = function
        | Empty -> None
        | NonEmpty q -> Some (List.head q.Front)

    static member tail : BootstrappedQueue<'T> -> BootstrappedQueue<'T> = function
        | Empty -> raise Exceptions.Empty
        | NonEmpty q ->
            let lenfm = q.FrontAndSuspensionsLength - 1
            let f' = List.tail q.Front
            NonEmptyBootstrappedQueue<'T>.create lenfm f' q.Suspensions q.RBackLength q.RBack
            |> BootstrappedQueue<'T>.checkQ

    static member tryGetTail : BootstrappedQueue<'T> -> BootstrappedQueue<'T> option = function
        | Empty -> None
        | NonEmpty q ->
            let lenfm = q.FrontAndSuspensionsLength - 1
            let f' = List.tail q.Front
            NonEmptyBootstrappedQueue<'T>.create lenfm f' q.Suspensions q.RBackLength q.RBack
            |> BootstrappedQueue<'T>.checkQ
            |> Some

    static member length : BootstrappedQueue<'T> -> int = function
        | Empty -> 0
        | NonEmpty q -> q.FrontAndSuspensionsLength + q.RBackLength

    static member ofList (l:List<'T>) : BootstrappedQueue<'T> = 
        let b0 = BootstrappedQueue.Empty
        NonEmptyBootstrappedQueue<'T>.create (l.Length) l b0 0 [] |> NonEmpty

///O(1). Returns queue of no elements.
let empty = Empty

///O(1). Returns true if the queue has no elements
let isEmpty = function Empty -> true | _ -> false

///O(log* n). Returns a new queue with the element added to the end.
let inline snoc x queue = BootstrappedQueue.snoc x queue

///O(1), worst case. Returns the first element.
let inline head queue = BootstrappedQueue<'T>.head queue

///O(1), worst case.  Returns option first element.
let inline tryGetHead queue = BootstrappedQueue<'T>.tryGetHead queue

///O(log* n), worst case. Returns a new queue of the elements trailing the first element.
let inline tail queue = BootstrappedQueue<'T>.tail queue

///O(log* n), worst case. Returns option queue of the elements trailing the first element.
let inline tryGetTail queue = BootstrappedQueue<'T>.tryGetTail queue

///O(1). Returns the count of elememts.
let inline length queue = BootstrappedQueue<'T>.length queue

///O(1). Returns a queue of the list.
let inline ofList list = BootstrappedQueue<'T>.ofList list