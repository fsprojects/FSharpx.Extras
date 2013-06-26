// RealTime queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/01/07/real-time-queue
module FSharpx.DataStructures.RealTimeQueue

open FSharpx

type RealTimeQueue<'T> = {
    F: LazyList<'T> 
    R: Lazy<list<'T>>
    S: LazyList<'T> }

let empty<'T> : RealTimeQueue<'T> = { F = LazyList.empty; R = lazy []; S = LazyList.empty }

let isEmpty queue = LazyList.isEmpty queue.F

let rec rotate queue =
    match queue.F with
    | LazyList.Nil -> LazyList.cons (Lazy.force queue.R |> List.head) queue.S
    | LazyList.Cons (hd, tl) ->
        let x = Lazy.force queue.R
        let y = List.head x
        let ys = List.tail x
        let right = LazyList.cons y queue.S
        LazyList.cons hd (rotate { F = tl; R = lazy ys; S = right })

let rec exec queue =
    match queue.S with
    | LazyList.Nil ->
        let f' = rotate {queue with S = LazyList.empty}
        { F = f'; R = lazy []; S = f' }
    | LazyList.Cons (hd, tl) -> {queue with S = tl}

let snoc x queue = exec {queue with R = lazy (x::Lazy.force queue.R) }

let head queue =
    match queue.F with
    | LazyList.Nil -> raise Exceptions.Empty
    | LazyList.Cons (hd, tl) -> hd

let tryGetHead queue = 
    match queue.F with
    | LazyList.Nil -> None
    | LazyList.Cons (hd, tl) -> Some hd

let tail queue =
    match queue.F with
    | LazyList.Nil -> raise Exceptions.Empty
    | LazyList.Cons (hd, tl) -> exec {queue with F = tl }

let tryGetTail queue = 
    match queue.F with
    | LazyList.Nil -> None
    | LazyList.Cons (hd, tl) -> Some(exec {queue with F = tl })
