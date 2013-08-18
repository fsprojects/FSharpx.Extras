// originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/17/pairing-heap
//J.F. modified

namespace FSharpx.DataStructures

#nowarn "44"
open System.Collections
open System.Collections.Generic


/// PairingHeap performs extremely well in practice, however (according to Okasaki) it should be avoided for applications taking advantage of persistence.
/// Also according to Okasaki the time complexity of the heap functions in the PairingHeap implementation have "resisted" time complexity analysis. 
/// ofSeq: superior performance; insert: superior performance; tail: superior performance
[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
type PairingHeap<'a when 'a : comparison> =
    | E of bool
    | T of bool * 'a * list<PairingHeap<'a>>

    with

    static member private descending : PairingHeap<'a> -> bool = function 
        | E(m) -> m 
        | T(m, _, _) -> m

    static member private merge (h1 : PairingHeap<'a>) (h2 : PairingHeap<'a>) : PairingHeap<'a> = 
        match h1, h2 with
        | E(_), h -> h
        | h, E(_) -> h
        | T(m, x, xs), T(_, y, ys) ->
            if m then
                if x <= y then T(m, y, h1::ys) else T(m, x, h2::xs)
            else 
                if x <= y then T(m, x, h2::xs) else T(m, y, h1::ys)

    //http://lorgonblog.wordpress.com/2008/04/06/catamorphisms-part-two
    static member private foldHeap nodeF leafV (h : list<PairingHeap<'a>>) = 

        let rec loop (h : list<PairingHeap<'a>>) cont =
            match h with
            | T(_, a, h')::tl -> loop h'  (fun lacc ->  
                                    loop tl (fun racc -> 
                                    cont (nodeF a lacc racc))) 
            | _ -> cont leafV
        
        loop h (fun x -> x)

    static member private inOrder (h : list<PairingHeap<'a>>) = (PairingHeap.foldHeap (fun x l r acc -> l (x :: (r acc))) (fun acc -> acc) h) [] 
    static member private sumTree (h : list<PairingHeap<'a>>) = (PairingHeap.foldHeap (fun x l r acc -> l (r (acc + 1))) (fun acc -> acc) h) 0 
           
    static member private isEmpty : PairingHeap<'a> -> bool = function 
        | E(_) -> true 
        | _ -> false

    static member private tryMerge (h1: PairingHeap<'a>) (h2: PairingHeap<'a>) : PairingHeap<'a> option = 
        if (PairingHeap.descending h1) = (PairingHeap.descending h2) then  Some(PairingHeap.merge h1 h2)
        else None

    static member private insert (x: 'a) (h: PairingHeap<'a>) : PairingHeap<'a> = 
        PairingHeap.merge (T((PairingHeap.descending h), x, [])) h

    static member private head: PairingHeap<'a> -> 'a = function
        | E(_) -> raise Exceptions.Empty
        | T(_, x, _) -> x

    static member private tryGetHead: PairingHeap<'a>  -> 'a option = function
        | E(_) -> None
        | T( _, x, _) -> Some(x)
 
    static member internal ofSeq (isDescending: bool) (s:seq<'a>) : PairingHeap<'a> = 
        if Seq.isEmpty s then E(isDescending)
        //Seq.fold performs better than the CPS style mergePairs algorithm for ofSeq
        //and has the further advantage of being only one line of code
        else Seq.fold (fun (h : 'a PairingHeap) t -> (h.Insert t)) (E(isDescending)) s

    static member private tail :PairingHeap<'a> -> PairingHeap<'a> = function
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

    static member private tryGetTail : PairingHeap<'a> -> PairingHeap<'a> option = function
        | E(_) -> None
        | T(m,x, xs) -> 
            let rec mergePairs pairList isDescending = 
                match pairList with
                | [] -> E(isDescending)
                | [x] -> x
                | x::y::tl -> PairingHeap.merge (PairingHeap.merge x y) (mergePairs tl isDescending)    
            Some(mergePairs xs m)

    static member private tryUncons (h :PairingHeap<'a>) : ('a * PairingHeap<'a>) option =
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
    member this.Merge (xs : PairingHeap<'a>) = 
        if this.IsDescending = xs.IsDescending then PairingHeap.merge this xs
        else failwith "not same max or min"

    ///O(log n) amortized time. Returns heap option from merging two heaps.
    member this.TryMerge (xs : PairingHeap<'a>) = 
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

    interface IEnumerable<'a> with

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

    interface IHeap<PairingHeap<'a>, 'a> with
        
        member this.Count() = this.Length()

        member this.Head() = PairingHeap.head this

        member this.TryGetHead() = PairingHeap.tryGetHead this

        member this.Insert (x : 'a) = PairingHeap.insert x this

        member this.IsEmpty = PairingHeap.isEmpty this

        member this.IsDescending = PairingHeap.descending this

        member this.Length() = this.Length() 

        member this.Merge (xs : PairingHeap<'a>) = PairingHeap.merge this xs

        member this.TryMerge (xs : PairingHeap<'a>)  = 
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

    interface IPriorityQueue<'a>

        with
        member this.IsEmpty = this.IsEmpty
        member this.Insert element = this.Insert element :> IPriorityQueue<'a>
        member this.TryPeek() = this.TryGetHead()
        member this.Peek() = this.Head()

        member this.TryPop() = 
            match this.TryUncons() with
            | Some(element,newHeap) -> Some(element,newHeap  :> IPriorityQueue<'a>)
            | None -> None

        member this.Pop() = 
            let element,newHeap = this.Uncons()
            element,(newHeap  :> IPriorityQueue<'a>)

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PairingHeap =   
    //pattern discriminator

    let (|Cons|Nil|) (h: PairingHeap<'a>) = match h.TryUncons() with Some(a,b) -> Cons(a,b) | None -> Nil
  
    ///O(1) Returns a empty heap.
    let inline empty (maximalist: bool) = E(maximalist)

    ///O(1) worst case. Returns the min or max element.
    let inline head (xs: PairingHeap<'a>)  = xs.Head()

    ///O(1) worst case. Returns option first min or max element.
    let inline tryGetHead (xs: PairingHeap<'a>)  = xs.TryGetHead()

    ///O(log n) amortized time. Returns a new heap with the element inserted.
    let inline insert x (xs: PairingHeap<'a>) = xs.Insert x   

    ///O(1) Returns true if the heap has no elements.
    let inline isEmpty (xs: PairingHeap<'a>) = xs.IsEmpty

    ///O(1). Returns true if the heap has max element at head.
    let inline isDescending (xs: PairingHeap<'a>) = xs.IsDescending

    ///O(n). Returns the count of elememts.
    let inline length (xs: PairingHeap<'a>) = xs.Length() 

    ///O(log n) amortized time. Returns heap from merging two heaps, both must have same descending.
    let inline merge (xs: PairingHeap<'a>) (ys: PairingHeap<'a>) = xs.Merge ys

    ///O(log n) amortized time. Returns heap option from merging two heaps.
    let inline tryMerge (xs: PairingHeap<'a>) (ys: PairingHeap<'a>) = xs.TryMerge ys

    ///O(n). Returns heap from the sequence.
    let ofSeq maximalist s = PairingHeap.ofSeq maximalist s

    ///O(log n) amortized time. Returns a new heap of the elements trailing the head.
    let inline tail (xs: PairingHeap<'a>) = xs.Tail()

    ///O(log n) amortized time. Returns option heap of the elements trailing the head.
    let inline tryGetTail (xs: PairingHeap<'a>) = xs.TryGetTail()

    ///O(log n) amortized time. Returns the head element and tail.
    let inline uncons (xs: PairingHeap<'a>) = xs.Uncons()

    ///O(log n) amortized time. Returns option head element and tail.
    let inline tryUncons (xs: PairingHeap<'a>) = xs.TryUncons()