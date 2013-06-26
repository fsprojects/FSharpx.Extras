// originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/17/pairing-heap
//J.F. modified

namespace FSharpx.Collections.Experimental

open System.Collections
open System.Collections.Generic


/// PairingHeap performs extremely well in practice, however (according to Okasaki) it should be avoided for applications taking advantage of persistence.
/// Also according to Okasaki the time complexity of the heap functions in the PairingHeap implementation have "resisted" time complexity analysis. 
/// ofSeq: superior performance; insert: superior performance; tail: superior performance
type PairingHeap<'T when 'T : comparison> =
    | E of bool
    | T of bool * 'T * list<PairingHeap<'T>>

    static member private descending : PairingHeap<'T> -> bool = function 
        | E(m) -> m 
        | T(m, _, _) -> m

    static member private merge (h1 : PairingHeap<'T>) (h2 : PairingHeap<'T>) : PairingHeap<'T> = 
        match h1, h2 with
        | E(_), h -> h
        | h, E(_) -> h
        | T(m, x, xs), T(_, y, ys) ->
            if m then
                if x <= y then T(m, y, h1::ys) else T(m, x, h2::xs)
            else 
                if x <= y then T(m, x, h2::xs) else T(m, y, h1::ys)

    //http://lorgonblog.wordpress.com/2008/04/06/catamorphisms-part-two
    static member private foldHeap nodeF leafV (h : list<PairingHeap<'T>>) = 

        let rec loop (h : list<PairingHeap<'T>>) cont =
            match h with
            | T(_, a, h')::tl -> loop h'  (fun lacc ->  
                                    loop tl (fun racc -> 
                                    cont (nodeF a lacc racc))) 
            | _ -> cont leafV
        
        loop h (fun x -> x)

    static member private inOrder (h : list<PairingHeap<'T>>) = (PairingHeap.foldHeap (fun x l r acc -> l (x :: (r acc))) (fun acc -> acc) h) [] 
    static member private sumTree (h : list<PairingHeap<'T>>) = (PairingHeap.foldHeap (fun x l r acc -> l (r (acc + 1))) (fun acc -> acc) h) 0 
           
    static member private isEmpty : PairingHeap<'T> -> bool = function 
        | E(_) -> true 
        | _ -> false

    static member private tryMerge (h1: PairingHeap<'T>) (h2: PairingHeap<'T>) : PairingHeap<'T> option = 
        if (PairingHeap.descending h1) = (PairingHeap.descending h2) then  Some(PairingHeap.merge h1 h2)
        else None

    static member private insert (x: 'T) (h: PairingHeap<'T>) : PairingHeap<'T> = 
        PairingHeap.merge (T((PairingHeap.descending h), x, [])) h

    static member private head: PairingHeap<'T> -> 'T = function
        | E(_) -> raise Exceptions.Empty
        | T(_, x, _) -> x

    static member private tryGetHead: PairingHeap<'T>  -> 'T option = function
        | E(_) -> None
        | T( _, x, _) -> Some(x)
 
    static member internal ofSeq (isDescending: bool) (s:seq<'T>) : PairingHeap<'T> = 
        if Seq.isEmpty s then E(isDescending)
        //Seq.fold performs better than the CPS style mergePairs algorithm for ofSeq
        //and has the further advantage of being only one line of code
        else Seq.fold (fun (h : 'T PairingHeap) t -> (h.Insert t)) (E(isDescending)) s

    static member private tail :PairingHeap<'T> -> PairingHeap<'T> = function
        | E(_) -> raise Exceptions.Empty
        | T(m,x, xs) -> 
            //non-tail recursive algorithm implemented directly from ML of Okasaki 1998 performs better in practice
            //especially for larger number of elements
            //and does not stackoverflow for heaps up to 1,000,000 elements
            let rec mergePairs pairList isDescending = 
                match pairList with
                | [] -> E(isDescending)
                | [x] -> x
                | x::y::tl -> PairingHeap.merge (PairingHeap.merge x y) (mergePairs tl isDescending)    
            mergePairs xs m
            //CPS version of mergePairs for comparison
//            let rec mergePairs pairList isDescending cont = 
//                match pairList with
//                | [] -> cont (E(isDescending))
//                | [x] -> cont x
//                | x::y::tl -> mergePairs tl isDescending (fun acc -> cont (PairingHeap.merge (PairingHeap.merge x y) acc))                   
//
//            mergePairs xs m (fun x -> x)

    static member private tryGetTail : PairingHeap<'T> -> PairingHeap<'T> option = function
        | E(_) -> None
        | T(m,x, xs) -> 
            let rec mergePairs pairList isDescending = 
                match pairList with
                | [] -> E(isDescending)
                | [x] -> x
                | x::y::tl -> PairingHeap.merge (PairingHeap.merge x y) (mergePairs tl isDescending)    
            Some(mergePairs xs m)

    static member private tryUncons (h :PairingHeap<'T>) : ('T * PairingHeap<'T>) option =
        match PairingHeap.tryGetHead h with
            | None -> None
            | Some(x) -> Some(x, (PairingHeap.tail h))
        
    ///O(1) worst case. Returns the min or max element.
    member this.Head() = PairingHeap.head this

    ///O(1) worst case. Returns option first min or max element.
    member this.TryGetHead() = PairingHeap.tryGetHead this

    ///O(log n) amortized time. Returns a new heap with the element inserted.
    member this.Insert x  = PairingHeap.insert x this

    ///O(1) Returns true if the heap has no elements.
    member this.IsEmpty = PairingHeap.isEmpty this

    ///O(1). Returns true if the heap has max element at head.
    member this.IsDescending = PairingHeap.descending this

    ///O(n). Returns the count of elememts.
    member this.Length() : int = 
        let lH = this::[]
        PairingHeap.sumTree lH

    ///O(log n) amortized time. Returns heap from merging two heaps, both must have same descending.
    member this.Merge (xs : PairingHeap<'T>) = 
        if this.IsDescending = xs.IsDescending then PairingHeap.merge this xs
        else failwith "not same max or min"

    ///O(log n) amortized time. Returns heap option from merging two heaps.
    member this.TryMerge (xs : PairingHeap<'T>) = 
        if this.IsDescending = xs.IsDescending then Some(PairingHeap.merge this xs)
        else None

    ///O(log n) amortized time. Returns a new heap of the elements trailing the head.
    member this.Tail() = PairingHeap.tail this
       
    ///O(log n) amortized time. Returns option heap of the elements trailing the head.
    member this.TryGetTail() = PairingHeap.tryGetTail this

    ///O(log n) amortized time. Returns the head element and tail.
    member this.Uncons() = 
        (PairingHeap.head this), (PairingHeap.tail this)

    ///O(log n) amortized time. Returns option head element and tail.
    member this.TryUncons() = PairingHeap.tryUncons this

    interface IEnumerable<'T> with

        member this.GetEnumerator() = 
            let e = 
                let listH = this::[]
                if this.IsDescending
//WARNING! List.sort |> List.rev significantly faster (caveat: on 32-bit Win 7) than List.sortwith...go figure!
//            PairingHeap.inOrder this |> List.sortWith (fun x y -> if (x > y) then -1
//                                                                             else 
//                                                                               if (x = y) then 0
//                                                                               else 1) |> List.fold f state
                then PairingHeap.inOrder listH |> List.sort |> List.rev |> List.toSeq
                else PairingHeap.inOrder listH |> List.sort |> List.toSeq

            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

    interface IHeap<PairingHeap<'T>, 'T> with
        
        member this.Count() = this.Length()

        member this.Head() = PairingHeap.head this

        member this.TryGetHead() = PairingHeap.tryGetHead this

        member this.Insert (x : 'T) = PairingHeap.insert x this

        member this.IsEmpty = PairingHeap.isEmpty this

        member this.IsDescending = PairingHeap.descending this

        member this.Length() = this.Length() 

        member this.Merge (xs : PairingHeap<'T>) = PairingHeap.merge this xs

        member this.TryMerge (xs : PairingHeap<'T>)  = 
            match PairingHeap.tryMerge this xs with
            | None -> None
            | Some(xs) -> Some(xs)

        member this.Tail() = PairingHeap.tail this

        member this.TryGetTail() =
            match PairingHeap.tryGetTail this with
            | None -> None
            | Some(xs) -> Some(xs)

        member this.Uncons() = 
            (PairingHeap.head this), (PairingHeap.tail this) 

        member this.TryUncons() =
            match PairingHeap.tryUncons this with
            | None -> None
            | Some(x, xs) -> Some(x, xs)

    interface IPriorityQueue<'T> with

        member this.IsEmpty = this.IsEmpty
        member this.Insert element = this.Insert element :> IPriorityQueue<'T>
        member this.TryPeek() = this.TryGetHead()
        member this.Peek() = this.Head()

        member this.TryPop() = 
            match this.TryUncons() with
            | Some(element,newHeap) -> Some(element,newHeap  :> IPriorityQueue<'T>)
            | None -> None

        member this.Pop() = 
            let element,newHeap = this.Uncons()
            element,(newHeap  :> IPriorityQueue<'T>)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PairingHeap =   
    //pattern discriminator

    let (|Cons|Nil|) (h: PairingHeap<'T>) = match h.TryUncons() with Some(a,b) -> Cons(a,b) | None -> Nil
  
    ///O(1) Returns a empty heap.
    let inline empty (maximalist: bool) = E(maximalist)

    ///O(1) worst case. Returns the min or max element.
    let inline head (xs: PairingHeap<'T>)  = xs.Head()

    ///O(1) worst case. Returns option first min or max element.
    let inline tryGetHead (xs: PairingHeap<'T>)  = xs.TryGetHead()

    ///O(log n) amortized time. Returns a new heap with the element inserted.
    let inline insert x (xs: PairingHeap<'T>) = xs.Insert x   

    ///O(1) Returns true if the heap has no elements.
    let inline isEmpty (xs: PairingHeap<'T>) = xs.IsEmpty

    ///O(1). Returns true if the heap has max element at head.
    let inline isDescending (xs: PairingHeap<'T>) = xs.IsDescending

    ///O(n). Returns the count of elememts.
    let inline length (xs: PairingHeap<'T>) = xs.Length() 

    ///O(log n) amortized time. Returns heap from merging two heaps, both must have same descending.
    let inline merge (xs: PairingHeap<'T>) (ys: PairingHeap<'T>) = xs.Merge ys

    ///O(log n) amortized time. Returns heap option from merging two heaps.
    let inline tryMerge (xs: PairingHeap<'T>) (ys: PairingHeap<'T>) = xs.TryMerge ys

    ///O(n). Returns heap from the sequence.
    let ofSeq maximalist s = PairingHeap.ofSeq maximalist s

    ///O(log n) amortized time. Returns a new heap of the elements trailing the head.
    let inline tail (xs: PairingHeap<'T>) = xs.Tail()

    ///O(log n) amortized time. Returns option heap of the elements trailing the head.
    let inline tryGetTail (xs: PairingHeap<'T>) = xs.TryGetTail()

    ///O(log n) amortized time. Returns the head element and tail.
    let inline uncons (xs: PairingHeap<'T>) = xs.Uncons()

    ///O(log n) amortized time. Returns option head element and tail.
    let inline tryUncons (xs: PairingHeap<'T>) = xs.TryUncons()