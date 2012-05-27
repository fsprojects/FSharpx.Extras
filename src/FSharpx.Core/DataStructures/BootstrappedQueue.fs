// bootstrapped queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/18/bootstrapped-queue
module FSharpx.DataStructures.BootstrappedQueue

open FSharpx

type nonempty_t<'a> = {
    FrontAndSuspensionsLength : int
    Front : list<'a>
    Suspensions : t<Lazy<list<'a>>>
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

and t<'a> = E | Q of nonempty_t<'a> with
    //polymorphic recursion cannot be achieved through let-bound functions
    //hence we use static member methods 

    static member checkQ (q:nonempty_t<'a>) =
        if q.RBackLength <= q.FrontAndSuspensionsLength then
        t.checkF q
        else
        let susp = t.snoc (lazy List.rev q.RBack) q.Suspensions
        nonempty_t<'a>.create (q.FrontAndSuspensionsLength + q.RBackLength) q.Front susp 0 []
        |> t.checkF 

    static member checkF (q:nonempty_t<'a>) : t<'a> =
        match q.Front, q.Suspensions with
        | [], E -> E
        | [], m ->
            let f = t.head m |> Lazy.force
            let susp = t.tail m
            Q <| nonempty_t<'a>.create q.FrontAndSuspensionsLength f susp q.RBackLength q.RBack
        | _ -> Q q

    static member snoc (x:'a) : t<'a> -> t<'a> = function
        | E -> Q <| nonempty_t<'a>.create 1 [x] E 0 []
        | Q q ->
            let lenr = q.RBackLength+1
            let r = x::q.RBack
            nonempty_t<'a>.create q.FrontAndSuspensionsLength q.Front q.Suspensions lenr r
            |> t<'a>.checkQ 

    static member head : t<'a> -> 'a = function
        | E -> raise Exceptions.Empty
        | Q q -> List.head q.Front

    static member tryGetHead : t<'a> -> 'a option = function
        | E -> None
        | Q q -> Some (List.head q.Front)

    static member tail : t<'a> -> t<'a> = function
        | E -> raise Exceptions.Empty
        | Q q ->
            let lenfm = q.FrontAndSuspensionsLength - 1
            let f' = List.tail q.Front
            nonempty_t<'a>.create lenfm f' q.Suspensions q.RBackLength q.RBack
            |> t<'a>.checkQ

    static member tryGetTail : t<'a> -> t<'a> option = function
        | E -> None
        | Q q ->
            let lenfm = q.FrontAndSuspensionsLength - 1
            let f' = List.tail q.Front
            nonempty_t<'a>.create lenfm f' q.Suspensions q.RBackLength q.RBack
            |> t<'a>.checkQ
            |> Some

let empty = E

let isEmpty = function E -> true | _ -> false

let head q = t.head q

let tail q = t.tail q

let snoc = t.snoc

let inline tryGetHead queue = t<'a>.tryGetHead queue
let inline tryGetTail queue = t<'a>.tryGetTail queue