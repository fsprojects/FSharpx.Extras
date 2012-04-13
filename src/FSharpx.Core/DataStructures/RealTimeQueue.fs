// RealTime queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/01/07/real-time-queue
namespace FSharpx.DataStructures

open FSharpx

module RealTimeQueue =
    type t<'a> = LazyList<'a> * Lazy<list<'a>> * LazyList<'a>

    let empty<'a> : t<'a> = LazyList.empty, lazy [], LazyList.empty

    let isEmpty (x, _, _) = LazyList.isEmpty x

    let rec rotate (a, x, b) =
        match a with
        | LazyList.Nil -> LazyList.cons (Lazy.force x |> List.head) b
        | LazyList.Cons (hd, tl) ->
            let x = Lazy.force x
            let y = List.head x
            let ys = List.tail x
            let right = LazyList.cons y b
            LazyList.cons hd (rotate (tl, lazy ys, right))

    let rec exec (f, r, right) =
        match right with
        | LazyList.Nil ->
            let f' = rotate (f, r, LazyList.empty)
            f', lazy [], f'
        | LazyList.Cons (hd, tl) -> f, r, tl

    let singleton x = LazyList.empty, lazy [x], LazyList.empty

    let snoc x (f, r, s) = exec (f, lazy (x::Lazy.force r), s)

    let head ((a, x, b):t<'a>) =
        match a with
        | LazyList.Nil -> raise Exceptions.Empty
        | LazyList.Cons (hd, tl) -> hd

    let tail ((a, x, b):t<'a>) =
        match a with
        | LazyList.Nil -> raise Exceptions.Empty
        | LazyList.Cons (hd, tl) -> exec (tl, x, b)