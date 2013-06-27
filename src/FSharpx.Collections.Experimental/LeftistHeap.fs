// originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/03/leftist-heap
//J.F. modified

namespace FSharpx.Collections.Experimental

open System.Collections
open System.Collections.Generic

type LeftistHeap<'T when 'T : comparison> =
    | E of bool
    | T of bool * int * int * 'T * LeftistHeap<'T> * LeftistHeap<'T> 

    static member private rank : LeftistHeap<'T> -> int = function 
        | E(_) -> 0 
        | T(_, _, r, _, _, _) -> r

    static member private make (x: 'T) (a: LeftistHeap<'T>) (b: LeftistHeap<'T>) : LeftistHeap<'T> =
        if LeftistHeap.rank a > LeftistHeap.rank b then
          T((a.IsDescending), (a.Length + b.Length + 1), LeftistHeap.rank b + 1, x, a, b)
        else
          T((a.IsDescending), (a.Length + b.Length + 1), LeftistHeap.rank a + 1, x, b, a)

    static member private merge (h1: LeftistHeap<'T>) (h2: LeftistHeap<'T>) : LeftistHeap<'T> = 
        if (h1.IsDescending) = (h2.IsDescending) then
            match h1, h2 with
            | E(_), x | x, E(_) -> x
            | T(_, _, _, x, a1, b1), T(_, _, _, y, a2, b2) ->
                if (h1.IsDescending) then
                    if x < y then LeftistHeap.make y a2 (LeftistHeap.merge h1 b2)
                    else LeftistHeap.make x a1 (LeftistHeap.merge b1 h2)
                else
                    if x < y then LeftistHeap.make x a1 (LeftistHeap.merge b1 h2)
                    else LeftistHeap.make y a2 (LeftistHeap.merge h1 b2)
        else
            failwith "not same max or min"

    //http://lorgonblog.wordpress.com/2008/04/06/catamorphisms-part-two
    static member private foldHeap nodeF leafV (h : LeftistHeap<'T>) = 

        let rec Loop (h : LeftistHeap<'T>) cont = 
            match h with 
            | T(_, _, _, a, l, r) -> Loop l  (fun lacc ->  
                                    Loop r (fun racc -> 
                                    cont (nodeF a lacc racc))) 
            | E(_) -> cont leafV 
        Loop h (fun x -> x)

    static member private inOrder (h : LeftistHeap<'T>) = (LeftistHeap.foldHeap (fun x l r acc -> l (x :: (r acc))) (fun acc -> acc) h) [] 
           
    static member private isEmpty : LeftistHeap<'T> -> bool = function 
        | E(_) -> true 
        | _ -> false

    static member private tryMerge (h1: LeftistHeap<'T>) (h2: LeftistHeap<'T>) : LeftistHeap<'T> option = 
        if (h1.IsDescending) = (h2.IsDescending) then
            match h1, h2 with
            | E(_), x | x, E(_) -> Some(x)
            | T(_, _, _, x, a1, b1), T(_, _, _, y, a2, b2) ->
                if x < y then Some(LeftistHeap.make x a1 (LeftistHeap.merge b1 h2))
                else Some(LeftistHeap.make y a2 (LeftistHeap.merge h1 b2))
        else None

    static member private insert (x: 'T) (h: LeftistHeap<'T>) : LeftistHeap<'T> = 
        let isDescending = h.IsDescending
        LeftistHeap.merge (T(isDescending, 1, 1, x, E(isDescending), E(isDescending))) h

    static member private head: LeftistHeap<'T> -> 'T = function
        | E(_) -> raise Exceptions.Empty
        | T(_, _, _, x, _, _) -> x

    static member private tryGetHead: LeftistHeap<'T>  -> 'T option = function
        | E(_) -> None
        | T(_, _, _, x, _, _) -> Some(x)
 
    static member internal ofSeq (descending: bool) (s:seq<'T>) : LeftistHeap<'T> = 
        if Seq.isEmpty s then E(descending)
        else
            let x, _ = 
                Seq.fold (fun (acc, isDescending) elem -> (((T(isDescending, 1, 1, elem, E(isDescending), E(isDescending))))::acc), isDescending) ([], descending) s
    
            let pairWiseMerge (l: list<LeftistHeap<'T>>) =
                let rec loop (acc: list<LeftistHeap<'T>>) : list<LeftistHeap<'T>> -> list<LeftistHeap<'T>> = function
                    | h1::h2::tl -> loop ((LeftistHeap.merge h1 h2)::acc) tl
                    | h1::[] -> h1::acc
                    | [] -> acc

                loop [] l

            let rec loop : list<LeftistHeap<'T>> -> LeftistHeap<'T> = function
                | h::[] -> h
                | x -> loop (pairWiseMerge x)
                
            loop x             

    static member private tail : LeftistHeap<'T> -> LeftistHeap<'T> = function
        | E(_) -> raise Exceptions.Empty
        | T(_, _, _, _, a, b) -> LeftistHeap.merge a b

    static member private tryGetTail : LeftistHeap<'T> -> LeftistHeap<'T> option = function
        | E(_) -> None
        | T(_, _, _, _, a, b) -> Some(LeftistHeap.merge a b)

    static member private tryUncons (h :LeftistHeap<'T>) : ('T * LeftistHeap<'T>) option =
        match LeftistHeap.tryGetHead h with
            | None -> None
            | Some(x) -> Some(x, (LeftistHeap.tail h))
        
    ///O(1). Returns the min or max element.
    member this.Head = LeftistHeap.head this

    ///O(1). Returns option first min or max element.
    member this.TryGetHead = LeftistHeap.tryGetHead this

    ///O(log n). Returns a new heap with the element inserted.
    member this.Insert x  = LeftistHeap.insert x this

    ///O(1). Returns true if the heap has no elements.
    member this.IsEmpty = LeftistHeap.isEmpty this

    ///O(1). Returns true if the heap has max element at head.
    member this.IsDescending : bool = 
        match this with
        | E(m) -> m 
        |  T(m, _, _, _, _, _) -> m

    ///O(1). Returns the count of elememts.
    member this.Length : int = 
        match this with
        | E(_) -> 0
        | T(_, i, _, _, _, _) -> i

    ///O(log n). Returns heap from merging two heaps, both must have same isDescending.
    member this.Merge xs = LeftistHeap.merge this xs

    ///O(log n). Returns heap option from merging two heaps
    member this.TryMerge xs = LeftistHeap.tryMerge this xs

    ///O(log n) Returns a new heap of the elements trailing the head.
    member this.Tail() = LeftistHeap.tail this
       
    ///O(log n). Returns option heap of the elements trailing the head.
    member this.TryGetTail() = LeftistHeap.tryGetTail this

    ///O(log n). Returns the head element and tail.
    member this.Uncons() = 
        (LeftistHeap.head this), (LeftistHeap.tail this)

    ///O(log n). Returns option head element and tail.
    member this.TryUncons() = LeftistHeap.tryUncons this

    interface IHeap<LeftistHeap<'T>, 'T> with
        
        member this.Count() = this.Length

        member this.Head() = LeftistHeap.head this

        member this.TryGetHead() = LeftistHeap.tryGetHead this

        member this.Insert (x : 'T) = LeftistHeap.insert x this

        member this.IsEmpty = LeftistHeap.isEmpty this

        member this.IsDescending = this.IsDescending 

        member this.Length() = this.Length 

        member this.Merge (xs : LeftistHeap<'T>) = LeftistHeap.merge this xs

        member this.TryMerge (xs : LeftistHeap<'T>)  = 
            match LeftistHeap.tryMerge this xs with
            | None -> None
            | Some(xs) -> Some(xs)

        member this.Tail() = LeftistHeap.tail this

        member this.TryGetTail() =
            match LeftistHeap.tryGetTail this with
            | None -> None
            | Some(xs) -> Some(xs)

        member this.Uncons() = 
            (LeftistHeap.head this), (LeftistHeap.tail this) 

        member this.TryUncons() =
            match LeftistHeap.tryUncons this with
            | None -> None
            | Some(x, xs) -> Some(x, xs)

        member this.GetEnumerator() = 
            let e = 
                if this.IsDescending
//WARNING! List.sort |> List.rev significantly faster (caveat: on 32-bit Win 7) than List.sortwith...go figure!
//            LeftistHeap.inOrder this |> List.sortWith (fun x y -> if (x > y) then -1
//                                                                             else 
//                                                                               if (x = y) then 0
//                                                                               else 1) |> List.fold f state
                then LeftistHeap.inOrder this |> List.sort |> List.rev |> List.toSeq
                else LeftistHeap.inOrder this |> List.sort |> List.toSeq

            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator  

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LeftistHeap =   
    //pattern discriminator

    let (|Cons|Nil|) (h: LeftistHeap<'T>) = match h.TryUncons() with Some(a,b) -> Cons(a,b) | None -> Nil
  
    ///O(1). Returns a empty heap.
    let inline empty (descending: bool) = E(descending)

    ///O(1). Returns the min or max element.
    let inline head (xs: LeftistHeap<'T>)  = xs.Head

    ///O(1). Returns option first min or max element.
    let inline tryGetHead (xs: LeftistHeap<'T>)  = xs.TryGetHead

    ///O(log n). Returns a new heap with the element inserted.
    let inline insert x (xs: LeftistHeap<'T>) = xs.Insert x   

    ///O(1). Returns true if the heap has no elements.
    let inline isEmpty (xs: LeftistHeap<'T>) = xs.IsEmpty

    ///O(1). Returns true if the heap has max element at head.
    let inline isDescending (xs: LeftistHeap<'T>) = xs.IsDescending

    ///O(1). Returns the count of elememts.
    let inline length (xs: LeftistHeap<'T>) = xs.Length 

    ///O(log n). Returns heap from merging two heaps, both must have same isDescending.
    let inline merge (xs: LeftistHeap<'T>) (ys: LeftistHeap<'T>) = xs.Merge ys

    ///O(log n). Returns heap option from merging two heaps.
    let inline tryMerge (xs: LeftistHeap<'T>) (ys: LeftistHeap<'T>) = xs.TryMerge ys

    ///O(log n). Returns heap from the sequence.
    let ofSeq descending s = LeftistHeap.ofSeq descending s

    ///O(log n). Returns a new heap of the elements trailing the head.
    let inline tail (xs: LeftistHeap<'T>) = xs.Tail()

    ///O(log n). Returns option heap of the elements trailing the head.
    let inline tryGetTail (xs: LeftistHeap<'T>) = xs.TryGetTail()

    ///O(log n). Returns the head element and tail.
    let inline uncons (xs: LeftistHeap<'T>) = xs.Uncons()

    ///O(log n). Returns option head element and tail.
    let inline tryUncons (xs: LeftistHeap<'T>) = xs.TryUncons()