// BottomUp merge sort from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/01/07/bottom-up-merge-sort
module FSharpx.Collections.Experimental.BottomUpMergeSort

open FSharpx

type Sortable<'T> = {
    Size: int
    Segments: Lazy<list<list<'T>>> }

let rec merge xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x::tlx, y::tly ->
        if x <= y then
            x :: merge tlx ys
        else
            y :: merge xs tly

let empty<'T> : Sortable<'T> = { Size = 0; Segments = lazy [] }

let isEmpty x = x.Size = 0

let singleton x = { Size = 1; Segments = lazy [[x]] }

let rec addSeg seg segs size =
    if size % 2 = 0 then
        seg::segs
    else
        addSeg (merge seg (List.head segs)) (List.tail segs) (size / 2)

let add x y = { Size = y.Size + 1; Segments = lazy addSeg [x] (Lazy.force y.Segments) y.Size }

let rec mergeAll xs ys =
    match xs, ys with
    | xs, [] -> xs
    | xs, seg::segs -> mergeAll (merge xs seg) segs

let sort x = mergeAll [] (Lazy.force x.Segments)