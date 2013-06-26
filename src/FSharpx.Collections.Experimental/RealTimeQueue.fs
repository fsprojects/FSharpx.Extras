// RealTime queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/01/07/real-time-queue
module FSharpx.Collections.Experimental.RealTimeQueue

open FSharpx.Collections

type RealTimeQueue<'T> = {
    F: LazyList<'T> 
    R: list<'T>
    S: LazyList<'T> }

///O(1). Returns queue of no elements.
let empty<'T> : RealTimeQueue<'T> = { F = LazyList.empty; R = []; S = LazyList.empty }

///O(1). Returns true if the queue has no elements
let isEmpty queue = LazyList.isEmpty queue.F

let rec rotate queue =
    match queue.F with
    | LazyList.Nil -> LazyList.cons (queue.R |> List.head) queue.S
    | LazyList.Cons (hd, tl) ->
        let x = queue.R
        let y = List.head x
        let ys = List.tail x
        let right = LazyList.cons y queue.S
        LazyList.cons hd (rotate { F = tl; R = ys; S = right })

let rec exec queue =
    match queue.S with
    | LazyList.Nil ->
        let f' = rotate {queue with S = LazyList.empty}
        { F = f'; R = []; S = f' }
    | LazyList.Cons (hd, tl) -> {queue with S = tl}

///O(1), worst case. Returns a new queue with the element added to the end.
let snoc x queue = exec {queue with R = (x::queue.R) }

///O(1), worst case. Returns the first element.
let head queue =
    match queue.F with
    | LazyList.Nil -> raise Exceptions.Empty
    | LazyList.Cons (hd, tl) -> hd

///O(1), worst case.  Returns option first element.
let tryGetHead queue = 
    match queue.F with
    | LazyList.Nil -> None
    | LazyList.Cons (hd, tl) -> Some hd

///O(1), worst case. Returns a new queue of the elements trailing the first element.
let tail queue =
    match queue.F with
    | LazyList.Nil -> raise Exceptions.Empty
    | LazyList.Cons (hd, tl) -> exec {queue with F = tl }

///O(1), worst case. Returns option queue of the elements trailing the first element.
let tryGetTail queue = 
    match queue.F with
    | LazyList.Nil -> None
    | LazyList.Cons (hd, tl) -> Some(exec {queue with F = tl })
