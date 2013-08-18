// originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/05/binary-random-access-list
//J.F. modified

namespace FSharpx.DataStructures

#nowarn "44"
open System.Collections
open System.Collections.Generic

type TreeSBRAL<'a> =
    | Leaf of 'a
    | Node of 'a * TreeSBRAL<'a> * TreeSBRAL<'a>

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
type SkewBinaryRandomAccessList<'a> (randomAccessList) =

    member this.randomAccessList = randomAccessList

    static member private length : int * list<int * TreeSBRAL<'a>> -> int = function
        | len, [] -> len
        | len, (0, _)::ts -> SkewBinaryRandomAccessList.length (len, ts)
        | len, (i, _)::ts -> SkewBinaryRandomAccessList.length ((len + i), ts)

    static member private cons (x:'a) : list<int * TreeSBRAL<'a>> -> list<int * TreeSBRAL<'a>> = function
        | [] -> [(1, Leaf x)]
        | (w1, t1)::[] -> (1, Leaf x)::(w1, t1)::[]  //Not in Okasaki, maybe Standard ML does some wierd stuff I don't understand
        | (w1, t1)::(w2, t2)::ts' as ts ->
            if w1 = w2 then
                (1 + w1 + w2, Node(x, t1, t2)) :: ts'
            else
                (1, Leaf x) :: ts

    static member private head : list<int * TreeSBRAL<'a>> -> 'a = function
        | [] -> raise Exceptions.Empty
        | (1, Leaf x)::_ -> x
        | (_, Node(x, _, _))::_ -> x
        | _ -> failwith "should not get there"

    static member private tryGetHead : list<int * TreeSBRAL<'a>> -> 'a option = function
        | [] -> None
        | (1, Leaf x)::_ -> Some(x)
        | (_, Node(x, _, _))::_ -> Some(x)
        | _ -> failwith "should not get there"

    static member private lookupTree : int * int * TreeSBRAL<'a> -> 'a = function
        | 1, 0, Leaf x -> x
        | 1, i, Leaf x -> raise Exceptions.OutOfBounds
        | w, 0, Node(x, _, _) -> x
        | w, i, Node(x, t1, t2) ->
            if i < (w+1) / 2 then           //really think this is a bug in Okasaki (he has "if i < w / 2 then"), but maybe Standard ML does some wierd stuff I don't understand
                SkewBinaryRandomAccessList.lookupTree (w/2, i - 1, t1)
            else
                SkewBinaryRandomAccessList.lookupTree (w/2, i - 1 - w/2, t2)
        | _ -> failwith "should not get there"

    static member private tryLookupTree : int * int  * TreeSBRAL<'a> -> 'a option = function
        | 1, 0, Leaf x -> Some(x)
        | 1, i, Leaf x -> None
        | w, 0, Node(x, _, _) -> Some(x)
        | w, i, Node(x, t1, t2) ->
            if i < (w+1) / 2 then
              SkewBinaryRandomAccessList.tryLookupTree (w/2, i - 1, t1)
            else
              SkewBinaryRandomAccessList.tryLookupTree (w/2, i - 1 - (w/2), t2)
        | _ -> failwith "should not get there"

    static member private lookup : int * list<int * TreeSBRAL<'a>> -> 'a = function
        | i, [] -> raise Exceptions.OutOfBounds
        | i, (w, t)::ts ->
            if i < w then SkewBinaryRandomAccessList.lookupTree (w, i, t) else SkewBinaryRandomAccessList.lookup ((i - w), ts)

    static member private tryLookup : int * list<int * TreeSBRAL<'a>> -> 'a  option = function
        | i, [] -> None
        | i, (w, t)::ts ->
            if i < w then SkewBinaryRandomAccessList.tryLookupTree (w, i, t) else SkewBinaryRandomAccessList.tryLookup ((i - w), ts)

    static member private tail : list<int * TreeSBRAL<'a>> -> list<int * TreeSBRAL<'a>> = function
        | [] -> raise Exceptions.Empty
        | (1, Leaf _)::ts -> ts
        | (w, Node(_, t1, t2))::ts -> (w/2, t1) :: (w/2, t2) :: ts
        | _ -> failwith "should not get there"

    static member private uncons (l: list<int * TreeSBRAL<'a>>) : 'a * list<int * TreeSBRAL<'a>> = 
        (SkewBinaryRandomAccessList.head l), (SkewBinaryRandomAccessList.tail l)
 
    static member updateTree : int * int * 'a * TreeSBRAL<'a> -> TreeSBRAL<'a> = function
        | 1, 0, y, Leaf x -> Leaf y
        | 1, i, y, Leaf x -> raise Exceptions.OutOfBounds
        | w, 0, y, Node(x, t1, t2) -> Node(y, t1, t2)
        | w, i, y, Node(x, t1, t2) ->
            if i < (w+1) / 2 then
              Node(x, SkewBinaryRandomAccessList.updateTree (w/2, i-1, y, t1) , t2)
            else
              Node(x, t1, SkewBinaryRandomAccessList.updateTree (w/2, i - 1 - w/2, y, t2))
        | _ -> failwith "should not get there"


    static member tryUpdateTree : int * int * 'a * TreeSBRAL<'a> -> TreeSBRAL<'a> option = function
        | 1, 0, y, Leaf x -> Some(Leaf y)
        | 1, i, y, Leaf x -> None
        | w, 0, y, Node(x, t1, t2) -> Some(Node(y, t1, t2))
        | w, i, y, Node(x, t1, t2) ->
            if i < (w+1) / 2 then
              Some(Node(x, (SkewBinaryRandomAccessList.tryUpdateTree (w/2, i - 1, y, t1)).Value , t2))
            else
              Some(Node(x, t1, (SkewBinaryRandomAccessList.tryUpdateTree (w/2, i - 1 - w/2, y, t2)).Value))
        | _ -> failwith "should not get there"

    static member update i y : list<int * TreeSBRAL<'a>>  -> list<int * TreeSBRAL<'a>> = function
        | []  -> raise Exceptions.OutOfBounds
        | (w, t)::ts ->
            if i < w then
              (w, SkewBinaryRandomAccessList.updateTree(w, i, y, t)) :: ts
            else
              (w, t) :: SkewBinaryRandomAccessList.update (i - w) y ts

    static member tryUpdate i y : list<int * TreeSBRAL<'a>> -> list<int * TreeSBRAL<'a>> option = function
        | []  -> None
        | (w, t)::ts ->
            if i < w then
              Some((w, (SkewBinaryRandomAccessList.tryUpdateTree (w, i, y, t)).Value) :: ts)
            else
              Some((w, t) :: (SkewBinaryRandomAccessList.tryUpdate (i - w) y ts).Value)

    static member internal ofSeq (s:seq<'a>) : SkewBinaryRandomAccessList<'a> = 
        if Seq.isEmpty s then SkewBinaryRandomAccessList([])
        else
            let a = Array.ofSeq s
            let rec loop (acc: list<int * TreeSBRAL<'a>>) dec (a': array<'a>) =
                if dec < 0 then SkewBinaryRandomAccessList(acc)
                else loop (SkewBinaryRandomAccessList.cons  a'.[dec] acc) (dec - 1) a'

            loop [] (a.Length - 1) a

    ///O(1), worst case. Returns a new random access list with the element added to the beginning.
    member this.Cons (x:'a)  = SkewBinaryRandomAccessList(SkewBinaryRandomAccessList.cons x randomAccessList)

    ///O(1), worst case. Returns the first element.
    member this.Head = SkewBinaryRandomAccessList.head randomAccessList

    ///O(1), worst case. Returns option first element.
    member this.TryGetHead = SkewBinaryRandomAccessList.tryGetHead randomAccessList

    ///O(1). Returns true if the random access list has no elements.
    member this.IsEmpty = 
        match randomAccessList with
        | [] -> true
        | _ -> false

    ///O(log n) Returns the count of elememts.
    member this.Length() = SkewBinaryRandomAccessList.length (0, randomAccessList)

    ///O(log n), worst case. Returns element by index.
    member this.Lookup (i:int) = SkewBinaryRandomAccessList.lookup (i, randomAccessList)

    ///O(log n), worst case. Returns option element by index.
    member thie.TryLookup (i:int) = SkewBinaryRandomAccessList.tryLookup (i, randomAccessList)

    ///O(n). Returns random access list reversed.
    member this.Rev() =

        let rec loop : list<int * TreeSBRAL<'a>> * list<int * TreeSBRAL<'a>> -> SkewBinaryRandomAccessList<'a>  = function
            | acc, [] -> SkewBinaryRandomAccessList(acc)  
            | acc, ral -> 
                let x, ts = SkewBinaryRandomAccessList.uncons ral
                loop ((SkewBinaryRandomAccessList.cons x acc), ts)
            
        loop ([], randomAccessList)

    ///O(1), worst case. Returns a new random access list of the elements trailing the first element.
    member this.Tail = SkewBinaryRandomAccessList(SkewBinaryRandomAccessList.tail randomAccessList)

    ///O(1), worst case. Returns a option random access list of the elements trailing the first element.
    member this.TryGetTail =
        match randomAccessList with
        | [] -> None
        | (1, Leaf _)::ts -> Some(SkewBinaryRandomAccessList(ts))
        | (w, Node(_, t1, t2))::ts -> Some(SkewBinaryRandomAccessList((w/2, t1) :: (w/2, t2) :: ts))
        | _ -> failwith "should not get there"

    ///O(1), worst case. Returns the first element and tail.
    member this.Uncons =
        this.Head, this.Tail

    ///O(1), worst case. Returns the option first element and tail.
    member this.TryUncons =
        match SkewBinaryRandomAccessList.tryGetHead randomAccessList with
        | None -> None
        | Some(x) -> Some(x, this.Tail) 

    ///O(log n), worst case. Returns random access list with element updated by index.
    member this.Update i y = SkewBinaryRandomAccessList(SkewBinaryRandomAccessList.update i y randomAccessList)
        
    ///O(log n), worst case. Returns option random access list with element updated by index.
    member this.TryUpdate i y =
        match SkewBinaryRandomAccessList.tryUpdate i y randomAccessList with
        | None -> None
        | Some(ts) -> Some(SkewBinaryRandomAccessList(ts))

    interface IRandomAccessList<'a> with

        member this.Cons (x : 'a) = this.Cons x :> _

        member this.Count() = this.Length()

        member this.Head = this.Head

        member this.TryGetHead = this.TryGetHead

        member this.IsEmpty = this.IsEmpty

        member this.Length() = this.Length()

        member this.Lookup i = this.Lookup i

        member this.TryLookup i = this.TryLookup i

        member this.Rev() = this.Rev() :> _

        member this.Tail = this.Tail :> _

        member this.TryGetTail =
            match this.TryGetTail with
            | None -> None
            | Some(xs) -> Some(xs :> _)

        member this.Uncons = 
            let x, ts = this.Uncons 
            x, ts :> _

        member this.TryUncons =
            match this.TryUncons  with
            | None -> None
            | Some(x, ts) -> Some(x, ts :> _)

        member this.Update i y = this.Update i y :> _
        
        member this.TryUpdate i y =
            match this.TryUpdate i y with
            | None -> None
            | Some(ts) -> Some(ts :> _)

        member this.GetEnumerator() = 
            let e = seq {
                match this.TryUncons with
                | None -> () 
                | Some(x, ts) ->
                    yield x 
                    yield! ts}
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator  

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SkewBinaryRandomAccessList =   
    //pattern discriminator

    let (|Cons|Nil|) (l: SkewBinaryRandomAccessList<'a>) = match l.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil
  
    ///O(1), worst case. Returns a new random access list with the element added to the beginning.
    let inline cons x (xs: SkewBinaryRandomAccessList<'a>) = xs.Cons x   
  
    ///O(1), worst case. Returns the first element.
    let inline head (xs: SkewBinaryRandomAccessList<'a>)  = xs.Head

    ///O(1), worst case. Returns option first element.
    let inline tryGetHead (xs: SkewBinaryRandomAccessList<'a>)  = xs.TryGetHead

    ///returns a empty random access list.
    let inline empty() = SkewBinaryRandomAccessList<'a>([])

    ///O(1). Returns true if the random access list has no elements.
    let inline isEmpty (xs: SkewBinaryRandomAccessList<'a>) = xs.IsEmpty

    ///O(log n). Returns the count of elememts.
    let inline length (xs: SkewBinaryRandomAccessList<'a>) = xs.Length() 

    ///O(log n), worst case. Returns element by index.
    let inline lookup i (xs: SkewBinaryRandomAccessList<'a>) = xs.Lookup i 

    ///O(log n), worst case. Returns option element by index.
    let inline tryLookup i (xs: SkewBinaryRandomAccessList<'a>) = xs.TryLookup i
    
    ///O(n) Returns random access list from the sequence.
    let ofSeq s = SkewBinaryRandomAccessList.ofSeq s

    ///O(n). Returns random access list reversed.
    let inline rev (xs: SkewBinaryRandomAccessList<'a>) = xs.Rev()

    ///O(1), worst case. Returns a new random access list of the elements trailing the first element.
    let inline tail (xs: SkewBinaryRandomAccessList<'a>) = xs.Tail

    ///O(1), worst case. Returns a option random access list of the elements trailing the first element.
    let inline tryGetTail (xs: SkewBinaryRandomAccessList<'a>) = xs.TryGetTail

    ///O(1), worst case. Returns the first element and tail.
    let inline uncons (xs: SkewBinaryRandomAccessList<'a>) = xs.Uncons

    ///O(1), worst case. Returns the option first element and tail.
    let inline tryUncons (xs: SkewBinaryRandomAccessList<'a>) = xs.TryUncons

    ///O(log n), worst case. Returns random access list with element updated by index.
    let inline update i y (xs: SkewBinaryRandomAccessList<'a>) = xs.Update i y

    ///O(log n), worst case. Returns option random access list with element updated by index.
    let inline tryUpdate i y (xs: SkewBinaryRandomAccessList<'a>) = xs.TryUpdate i y