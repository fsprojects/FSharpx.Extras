// originally published by Julien
// original implementation taken http://lepensemoi.free.fr/index.php/2009/12/10/binomial-heap

//J.F. modified

namespace FSharpx.DataStructures

open System.Collections
open System.Collections.Generic

type BinomialTree<'a> = Node of (int * 'a * list<BinomialTree<'a>>)

type BinomialHeap<'a when 'a : comparison> (isMaximalist : bool, heap : list<BinomialTree<'a>>) =

    member private this.heap = heap

    ///returns true if the heap has max element at head, O(1)
    member this.IsMaximalist = isMaximalist

    ///returns the count of elememts, O(log n)
    member this.Length() = List.fold (fun acc (Node(r, _, _)) -> acc + int (2.0**(float r))) 0 this.heap

    static member internal  empty isMaximalist = BinomialHeap<'a>(isMaximalist, [])

    static member private  isEmpty : BinomialHeap<'a> -> bool = function 
        | h when h.heap.IsEmpty -> true 
        | _ -> false

    static member private  rank ((Node(r, _, _)) : BinomialTree<'a>) = r

    static member private  root ((Node(_, x, _)) : BinomialTree<'a>) = x

    static member private  link isMaximalist (Node(r, x1, xs1) as n1) (Node(_, x2, xs2) as n2) : BinomialTree<'a> =
        if x1 <= x2 then
            if isMaximalist then Node(r+1, x2, n1 :: xs2)
            else Node(r+1, x1, n2 :: xs1)
        else
            if isMaximalist then Node(r+1, x1, n2 :: xs1)
            else Node(r+1, x2, n1 :: xs2)

    static member private insertTree (isMaximalist : bool) (t : BinomialTree<'a>) (h : list<BinomialTree<'a>>) : list<BinomialTree<'a>> = 
        match h with
        | [] -> [t]
        | hd::tl as t' ->
            if BinomialHeap.rank t < BinomialHeap.rank hd then t::t'
            else BinomialHeap.insertTree isMaximalist (BinomialHeap.link isMaximalist t hd) tl

    static member private  insert isMaximalist x (h : list<BinomialTree<'a>>) =
        BinomialHeap.insertTree isMaximalist (Node(0, x, [])) h

    static member private merge isMaximalist (h1  : list<BinomialTree<'a>>) (h2  : list<BinomialTree<'a>>)  : list<BinomialTree<'a>> =
        match h1, h2 with
        | [], x -> x
        | x, [] -> x
        | (hd1::tl1), (hd2::tl2) ->
          if BinomialHeap.rank hd1 < BinomialHeap.rank hd2 
          then hd1 :: BinomialHeap.merge isMaximalist tl1 h2
          elif BinomialHeap.rank hd1 > BinomialHeap.rank hd2 then hd2 :: BinomialHeap.merge isMaximalist h1 tl2
          else BinomialHeap.insertTree isMaximalist (BinomialHeap.link isMaximalist hd1 hd2) (BinomialHeap.merge isMaximalist ( tl1) ( tl2))

    static member private removeHeadTree : bool * list<BinomialTree<'a>> -> (BinomialTree<'a> * list<BinomialTree<'a>>) = function
        | _, [] -> raise Exceptions.Empty
        | _, [t] -> t, []
        | isMaximalist, hd::tl ->
            let hd', tl'= BinomialHeap.removeHeadTree (isMaximalist, tl)
            if isMaximalist then
                if BinomialHeap.root hd > BinomialHeap.root hd' 
                then hd, tl 
                else hd', hd::tl'
            else
                if BinomialHeap.root hd <= BinomialHeap.root hd' 
                then hd, tl 
                else hd', hd::tl'

    static member private head isMaximalist (h : list<BinomialTree<'a>>) : 'a =
        let (Node(_, x, xs1), _) = BinomialHeap.removeHeadTree (isMaximalist, h)
        x

    static member internal ofSeq (maximalist: bool) (s:seq<'a>) : BinomialHeap<'a> = 
        if Seq.isEmpty s then BinomialHeap.empty maximalist
        else
            let x = 
                Seq.fold (fun acc elem -> [Node(0, elem, [])]::acc) [] s
    
            let pairWiseMerge (l: list<list<BinomialTree<'a>>>) =
                let rec loop (acc: list<list<BinomialTree<'a>>>) : list<list<BinomialTree<'a>>> -> list<list<BinomialTree<'a>>> = function
                    | h1::h2::tl -> loop ((BinomialHeap.merge maximalist h1 h2)::acc) tl
                    | h1::[] -> h1::acc
                    | [] -> acc

                loop [] l

            let rec loop : list<list<BinomialTree<'a>>> -> list<BinomialTree<'a>> = function
                | h::[] -> h
                | x -> loop (pairWiseMerge x)
                
            BinomialHeap<'a>(maximalist, (loop x)) 

    static member private tail isMaximalist (h : list<BinomialTree<'a>>) =
        let Node(_, x, xs1), xs2 = BinomialHeap.removeHeadTree (isMaximalist, h)
        BinomialHeap.merge isMaximalist (List.rev xs1) xs2

    static member private uncons isMaximalist (h : list<BinomialTree<'a>>) : 'a * BinomialHeap<'a> =
        let Node(_, x, xs1), xs2 = BinomialHeap.removeHeadTree (isMaximalist, h)
        x, BinomialHeap<'a>(isMaximalist, (BinomialHeap.merge isMaximalist (List.rev xs1) xs2))
    
    static member private foldHeap nodeF leafV (h : list<BinomialTree<'a>>) = 

        let rec loop (h : list<BinomialTree<'a>>) cont =
            match h with
            | Node(_, a, h')::tl -> loop h'  (fun lacc ->  
                                    loop tl (fun racc -> 
                                    cont (nodeF a lacc racc))) 
            | _ -> cont leafV
           
        loop h (fun x -> x)

    static member private inOrder (h : list<BinomialTree<'a>>) = (BinomialHeap.foldHeap (fun x l r acc -> l (x :: (r acc))) (fun acc -> acc) h) [] 

    ///returns the min or max element, O(log n)
    member this.Head() = BinomialHeap.head this.IsMaximalist this.heap

    ///returns option first min or max element, O(log n)
    member this.TryGetHead() = 
        if this.heap.IsEmpty then None
        else Some(BinomialHeap.head this.IsMaximalist this.heap)

    ///returns a new heap with the element inserted, O(log n)
    member this.Insert x  = BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.insert this.IsMaximalist x this.heap))

    ///returns true if the heap has no elements, O(1)
    member this.IsEmpty = BinomialHeap.isEmpty this

    ///returns heap from merging two heaps, both must have same isMaximalist, O(log n)
    member this.Merge (xs : BinomialHeap<'a>) : BinomialHeap<'a> = 
        if (this.IsMaximalist = xs.IsMaximalist) then
            BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.merge this.IsMaximalist this.heap xs.heap))
        else failwith "not same max or min"

    ///returns heap option from merging two heaps, O(log n)
    member this.TryMerge (xs : BinomialHeap<'a>) = 
        if (this.IsMaximalist <> xs.IsMaximalist) then None
        else Some(BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.merge this.IsMaximalist this.heap xs.heap)))

    ///returns a new heap of the elements trailing the head, O(log n)
    member this.Tail() = 
        BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.tail this.IsMaximalist this.heap))
       
    ///returns option heap of the elements trailing the head, O(log n)
    member this.TryGetTail() = 
        if this.heap.IsEmpty then None
        else Some(BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.tail this.IsMaximalist this.heap)))

    ///returns the head element and tail, O(log n)
    member this.Uncons() = BinomialHeap.uncons this.IsMaximalist this.heap

    ///returns option head element and tail, O(log n)
    member this.TryUncons() = 
        if this.heap.IsEmpty then None
        else Some(BinomialHeap.uncons this.IsMaximalist this.heap)

    interface IHeap<BinomialHeap<'a>, 'a> with
        
        member this.Count() = this.Length()

        member this.Head() = BinomialHeap.head this.IsMaximalist this.heap

        member this.TryGetHead() = 
            if this.heap.IsEmpty then None
            else Some(BinomialHeap.head this.IsMaximalist this.heap)

        member this.Insert (x : 'a) = BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.insert this.IsMaximalist x this.heap))

        member this.IsEmpty = BinomialHeap.isEmpty this

        member this.IsMaximalist = this.IsMaximalist 

        member this.Length() = this.Length() 

        member this.Merge (xs : BinomialHeap<'a>) = 
            if (this.IsMaximalist = xs.IsMaximalist) then
                BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.merge this.IsMaximalist this.heap xs.heap))
            else failwith "not same max or min"

        member this.TryMerge (xs : BinomialHeap<'a>)  = 
            if (this.IsMaximalist <> xs.IsMaximalist) then None
            else Some(BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.merge this.IsMaximalist this.heap xs.heap)))

        member this.Tail() = BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.tail this.IsMaximalist this.heap))

        member this.TryGetTail() =
            if this.heap.IsEmpty then None
            else Some(BinomialHeap<'a>(this.IsMaximalist, (BinomialHeap.tail this.IsMaximalist this.heap)))

        member this.Uncons() = BinomialHeap.uncons this.IsMaximalist this.heap

        member this.TryUncons() =
            if this.heap.IsEmpty then None
            else Some(BinomialHeap.uncons this.IsMaximalist this.heap)

        member this.GetEnumerator() = 
            let e = 
                if this.IsMaximalist
//WARNING! List.sort |> List.rev significantly faster (caveat: on 32-bit Win 7) than List.sortwith...go figure!
//            BinomialHeap.inOrder this |> List.sortWith (fun x y -> if (x > y) then -1
//                                                                             else 
//                                                                               if (x = y) then 0
//                                                                               else 1) |> List.fold f state
                then BinomialHeap.inOrder this.heap |> List.sort |> List.rev |> List.toSeq
                else BinomialHeap.inOrder this.heap |> List.sort |> List.toSeq

            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator  

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

module BinomialHeap = 
    //pattern discriminator
    let (|Cons|Nil|) (h: BinomialHeap<'a>) = match h.TryUncons() with Some(a,b) -> Cons(a,b) | None -> Nil
  
    ///returns a empty heap, O(1)
    let empty (maximalist: bool) = BinomialHeap.empty maximalist

    ///returns the min or max element, O(log n)
    let inline head (xs: BinomialHeap<'a>)  = xs.Head()

    ///returns option first min or max element, O(log n)
    let inline tryGetHead (xs: BinomialHeap<'a>)  = xs.TryGetHead()

    ///returns a new heap with the element inserted, O(log n)
    let inline insert x (xs: BinomialHeap<'a>) = xs.Insert x   

    ///returns true if the heap has no elements, O(1)
    let inline isEmpty (xs: BinomialHeap<'a>) = xs.IsEmpty

    ///returns true if the heap has max element at head, O(1)
    let inline isMaximalist (xs: BinomialHeap<'a>) = xs.IsMaximalist

    ///returns the count of elememts, O(log n)
    let inline length (xs: BinomialHeap<'a>) = xs.Length() 

    ///returns heap from merging two heaps, both must have same isMaximalist, O(log n)
    let inline merge (xs: BinomialHeap<'a>) (ys: BinomialHeap<'a>) = xs.Merge ys

    ///returns heap option from merging two heaps, O(log n)
    let inline tryMerge (xs: BinomialHeap<'a>) (ys: BinomialHeap<'a>) = xs.TryMerge ys

    ///returns heap from the sequence, O(log n)
    let ofSeq maximalist s = BinomialHeap.ofSeq maximalist s

    ///returns a new heap of the elements trailing the head, O(log n)
    let inline tail (xs: BinomialHeap<'a>) = xs.Tail()

    ///returns option heap of the elements trailing the head, O(log n)
    let inline tryGetTail (xs: BinomialHeap<'a>) = xs.TryGetTail()

    ///returns the head element and tail, O(log n)
    let inline uncons (xs: BinomialHeap<'a>) = xs.Uncons()

    ///returns option head element and tail, O(log n)
    let inline tryUncons (xs: BinomialHeap<'a>) = xs.TryUncons()