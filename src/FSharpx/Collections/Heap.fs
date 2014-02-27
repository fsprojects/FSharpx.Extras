// based on pairing heap published by Okasaki
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/17/pairing-heap
//J.F. modified

namespace FSharpx.Collections

open FSharpx
open System.Collections
open System.Collections.Generic

type Heap<'T when 'T : comparison>(isDescending : bool, length : int, data : HeapData<'T> ) =
    let mutable hashCode = None
    member internal this.heapData = data
    member internal this.heapLength = length

    override this.GetHashCode() =
        match hashCode with
        | None ->
            let mutable hash = 1
            for x in this do
                hash <- 31 * hash + Unchecked.hash x
            hashCode <- Some hash
            hash
        | Some hash -> hash

    override this.Equals(other) =
        match other with
        | :? Heap<'T> as y -> 
            if this.Length <> y.Length then false 
            else
                if this.GetHashCode() <> y.GetHashCode() then false
                else Seq.forall2 (Unchecked.equals) this y
        | _ -> false

    static member private merge isDescending newLength (h1 : HeapData<'T>) (h2 : HeapData<'T>) : Heap<'T> = 
        match h1, h2 with
        | E, h -> Heap(isDescending, newLength, h)
        | h, E -> Heap(isDescending, newLength, h)
        | T(x, xs), T(y, ys) ->
            if isDescending then
                if x <= y then Heap(isDescending, newLength, T(y, h1::ys)) else Heap(isDescending, newLength, T(x, h2::xs))
            else 
                if x <= y then Heap(isDescending, newLength, T(x, h2::xs)) else Heap(isDescending, newLength, T(y, h1::ys))

    //http://lorgonblog.wordpress.com/2008/04/06/catamorphisms-part-two
    static member private foldHeap nodeF leafV (h : list<HeapData<'T>>) = 

        let rec loop (h : list<HeapData<'T>>) cont =
            match h with
            | T(a, h')::tl -> loop h'  (fun lacc ->  
                                    loop tl (fun racc -> 
                                    cont (nodeF a lacc racc))) 
            | _ -> cont leafV
        
        loop h (fun x -> x)

    static member private inOrder (h : list<HeapData<'T>>) = (Heap.foldHeap (fun x l r acc -> l (x :: (r acc))) (fun acc -> acc) h) [] 
 
    static member internal ofSeq (isDescending: bool) (s:seq<'T>) : Heap<'T> = 
        if Seq.isEmpty s then Heap(isDescending, 0, E)
        else
            let len, h' =
                 Seq.fold (fun (lnth, (h : 'T HeapData)) x -> 
                    match h with 
                    | E -> 1, T(x, [])
                    | T(y, ys) ->
                    if isDescending then
                        if x <= y then (lnth + 1), T(y, T(x, [])::ys) else (lnth + 1), T(x, T(y, ys)::[])
                    else 
                        if x <= y then (lnth + 1), T(x, T(y, ys)::[]) else (lnth + 1), T(y, T(x, [])::ys) ) (0,E) s
            Heap(isDescending, len, h')
        
    member this.Head = 
        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, _) -> x

    member this.TryHead = 
        match data with
        | E -> None
        | T(x, _) -> Some(x)

    member this.Insert x  = 
        Heap.merge isDescending (length + 1) (T(x, [])) data

    member this.IsEmpty = 
        match data with
        | E -> true 
        | _ -> false

    member this.IsDescending = isDescending

    member this.Length = length

    member this.Merge (xs : Heap<'T>) = 
        if isDescending = xs.IsDescending then Heap.merge isDescending (length + xs.heapLength) data xs.heapData
        else failwith "heaps to merge have different sort orders"

    member this.TryMerge (xs : Heap<'T>) = 
        if isDescending = xs.IsDescending then Some(Heap.merge isDescending (length + xs.heapLength) data xs.heapData)
        else None

    member this.Rev() = 
        if isDescending then Heap<'T>.ofSeq false (this :> seq<'T>)
        else  Heap<'T>.ofSeq true (this :> seq<'T>)

    member this.Tail() =

        let mergeData (h1 : HeapData<'T>) (h2 : HeapData<'T>) : HeapData<'T> = 
            match h1, h2 with
            | E, h -> h
            | h, E -> h
            | T(x, xs), T(y, ys) ->
                if isDescending then
                    if x <= y then T(y, h1::ys) else T(x, h2::xs)
                else 
                    if x <= y then T(x, h2::xs) else T(y, h1::ys)

        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, xs) -> 
            let rec mergePairs pairList = 
                match pairList with
                | [] ->  E
                | [x] -> x
                | x::y::tl -> mergeData (mergeData x y) (mergePairs tl)    
            Heap(isDescending, (length - 1), mergePairs xs)
       
    member this.TryTail() =
        match data with
        | E -> None
        | _ -> Some (this.Tail())

    member this.Uncons() = 
        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, xs) -> x, this.Tail() 

    member this.TryUncons() = 
        match data with
        | E -> None
        | T(x, xs) -> Some(x, this.Tail())

    interface IEnumerable<'T> with

        member this.GetEnumerator() = 
            let e = 
                let listH = data::[]
                if isDescending
                then Heap.inOrder listH |> List.sort |> List.rev |> List.toSeq
                else Heap.inOrder listH |> List.sort |> List.toSeq

            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

    interface IPriorityQueue<'T>

        with
        member this.IsEmpty = this.IsEmpty
        member this.Insert element = this.Insert element :> IPriorityQueue<'T>
        member this.TryPeek = this.TryHead
        member this.Peek = this.Head

        member this.TryPop() = 
            match this.TryUncons() with
            | Some(element,newHeap) -> Some(element,newHeap  :> IPriorityQueue<'T>)
            | None -> None

        member this.Pop() = 
            let element,newHeap = this.Uncons()
            element,(newHeap  :> IPriorityQueue<'T>)

and HeapData<'T when 'T : comparison> =
    | E 
    | T of 'T * list<HeapData<'T>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Heap =   
    //pattern discriminator

    let (|Cons|Nil|) (h: Heap<'T>) = match h.TryUncons() with Some(a,b) -> Cons(a,b) | None -> Nil
  
    let empty<'T when 'T : comparison> (isDescending: bool) = Heap<'T>(isDescending, 0, E)

    let inline head (xs: Heap<'T>)  = xs.Head

    let inline tryHead (xs: Heap<'T>)  = xs.TryHead

    let inline insert x (xs: Heap<'T>) = xs.Insert x   

    let inline isEmpty (xs: Heap<'T>) = xs.IsEmpty

    let inline isDescending (xs: Heap<'T>) = xs.IsDescending

    let inline length (xs: Heap<'T>) = xs.Length 

    let inline merge (xs: Heap<'T>) (ys: Heap<'T>) = xs.Merge ys

    let inline tryMerge (xs: Heap<'T>) (ys: Heap<'T>) = xs.TryMerge ys

    let ofSeq isDescending s  = Heap<'T>.ofSeq isDescending s 
    
    let inline rev (xs: Heap<'T>) = xs.Rev()

    let inline tail (xs: Heap<'T>) = xs.Tail()

    let inline tryTail (xs: Heap<'T>) = xs.TryTail()

    let inline toSeq (xs: Heap<'T>) = xs :> seq<'T>

    let inline uncons (xs: Heap<'T>) = xs.Uncons()

    let inline tryUncons (xs: Heap<'T>) = xs.TryUncons()

    let monoid<'a when 'a :comparison> = 
        { new Monoid<Heap<'a>>() with
            override x.Zero() = empty true
            override x.Combine(a,b) = merge a b }