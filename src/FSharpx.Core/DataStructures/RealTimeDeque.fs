//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/05/real-time-double-ended-queue

//jf -- added rev, ofSeq, ofSeqC, lookup, append, appendC, update, remove, tryUpdate, tryRemove
//   -- added standardized C of 2 for singleton, empty, and ofSeq based on my reading of Okasaki

//pattern discriminators Cons, Snoc, and Nil

namespace FSharpx.DataStructures

open FSharpx.Collections
open LazyListHelpr
open System.Collections
open System.Collections.Generic

type RealTimeDeque<'a>(c : int, frontLength : int, front : LazyList<'a>,  streamFront : LazyList<'a>,  rBackLength : int, rBack : LazyList<'a>, streamRBack : LazyList<'a>) = 
   
    member private this.c = c

    member private this.frontLength = frontLength

    member private this.front = front

    member private this.streamFront = streamFront

    member private this.rBackLength = rBackLength

    member private this.rBack = rBack

    member private this.streamRBack = streamRBack

    static member private length (q : RealTimeDeque<'a>) = q.frontLength + q.rBackLength

    static member private exec1 : LazyList<'a> -> LazyList<'a> = function
        | LazyList.Cons(x, s) -> s
        | s -> s

    static member private exec2 (x : LazyList<'a>) : LazyList<'a> = (RealTimeDeque.exec1 >> RealTimeDeque.exec1) x

    static member private check (q : RealTimeDeque<'a>) =

        let rec rotateDrop c f j r =

            let rec rotateRev c = function
                | LazyList.Nil, r, a -> LazyList.append (lLrev r) a
                | LazyList.Cons(x, f), r, a ->
                    let a' = lLdrop c r
                    let b' = LazyList.append (LazyList.take c r) a |> lLrev
                    LazyList.cons x (rotateRev c (f, a', b'))

            if j < c then
              rotateRev c (f, lLdrop j r, LazyList.empty)
            else
              match f with
              | LazyList.Cons(x, f') -> LazyList.cons x (rotateDrop c f' (j-c) (lLdrop c r))
              | _ -> failwith "should not get there"

        let n = RealTimeDeque.length q
        if q.frontLength > q.c * q.rBackLength + 1 then
          let i= n / 2
          let j = n - i
          let f' = LazyList.take i q.front
          let r' = rotateDrop q.c q.rBack i q.front
          new RealTimeDeque<'a>(q.c, i, f', f', j, r', r')
        elif q.rBackLength > q.c * q.frontLength + 1 then
          let j = n / 2
          let i = n - j
          let r' = LazyList.take j q.rBack
          let f' = rotateDrop q.c q.front j q.rBack
          new RealTimeDeque<'a>(q.c, i, f', f', j, r', r')
        else
          q

    static member private check2 q =
        let n = RealTimeDeque.length q
        if q.frontLength > q.c * q.rBackLength + 1 then
            let i= n / 2
            let j = n - i
            let f' = LazyList.take i q.front
            let r' = lLdrop i q.front |> lLrev |> LazyList.append q.rBack
            new RealTimeDeque<'a>(q.c, i, f', f', j, r', r')
        elif q.rBackLength > q.c * q.frontLength + 1 then
            let j = n / 2
            let i = n - j
            let r' = LazyList.take j q.rBack
            let f' = lLdrop j q.rBack |> lLrev |> LazyList.append q.front
            new RealTimeDeque<'a>(q.c, i, f', f', j, r', r')
        else
            new RealTimeDeque<'a>(q.c, q.frontLength, q.front, q.front, q.rBackLength, q.rBack, q.rBack)

    static member internal AppendC cC (xs:RealTimeDeque<'a>) (ys:RealTimeDeque<'a>) =
        let front2 = xs.frontLength + xs.rBackLength
        let front3 = xs.frontLength + xs.rBackLength + ys.frontLength
        let back2 = ys.frontLength + ys.rBackLength
        let back3 = xs.rBackLength + ys.frontLength + ys.rBackLength
        match (front2, front3, back2, back3) with 
        | a, b, c, d when (abs(xs.frontLength - d) <= abs(a - c)) && (abs(xs.frontLength - d) <= abs(a - ys.rBackLength) && (xs.frontLength > 0)) -> 
            new RealTimeDeque<'a>(cC, xs.frontLength, xs.front, LazyList.empty, 
                (xs.rBackLength + ys.frontLength + ys.rBackLength), (LazyList.append ys.rBack (LazyList.append  (lLrev ys.front) xs.rBack)), LazyList.empty)
            |> RealTimeDeque.check2
        | a, b, c, d when (abs(a - c) <= abs(xs.frontLength - d)) && (abs(a - c) <= abs(a - ys.rBackLength)) -> 
            new RealTimeDeque<'a>(cC, (xs.frontLength + xs.rBackLength), (LazyList.append xs.front (lLrev xs.rBack)), LazyList.empty, 
                (ys.frontLength + ys.rBackLength), (LazyList.append ys.rBack (lLrev ys.front)), LazyList.empty)
            |> RealTimeDeque.check2
        | a, b, c, d ->
            new RealTimeDeque<'a>(cC, (xs.frontLength + xs.rBackLength + ys.frontLength), (LazyList.append (LazyList.append xs.front (lLrev xs.rBack)) ys.front), LazyList.empty, 
                ys.rBackLength, ys.rBack, LazyList.empty)
            |> RealTimeDeque.check2

    static member internal Empty c =
        new RealTimeDeque<'a>(c, 0, (LazyList.empty), (LazyList.empty), 0, (LazyList.empty), (LazyList.empty)) 

    static member internal OfCatListsC c (xs : 'a list) (ys : 'a list) =
        new RealTimeDeque<'a>(c, xs.Length, (LazyList.ofList xs), LazyList.empty, ys.Length, (LazyList.ofList (List.rev ys)), LazyList.empty)
        |> RealTimeDeque.check2

    static member internal OfCatSeqsC c (xs : 'a seq) (ys : 'a seq) =
        new RealTimeDeque<'a>(c, (Seq.length xs), (LazyList.ofSeq xs), LazyList.empty, (Seq.length ys), (lLrev (LazyList.ofSeq ys)), LazyList.empty)
        |> RealTimeDeque.check2
   
    static member internal OfSeqC c (xs:seq<'a>) = 
        new RealTimeDeque<'a>(c, (Seq.length xs), (LazyList.ofSeq xs), LazyList.empty, 0, LazyList.empty, LazyList.empty)
        |> RealTimeDeque.check2

    ///O(1), worst case. Returns a new deque with the element added to the beginning.
    member this.Cons x =
        new RealTimeDeque<'a>(this.c, (this.frontLength+1), (LazyList.cons x this.front), (RealTimeDeque.exec1 this.streamFront), this.rBackLength, this.rBack, (RealTimeDeque.exec1 this.streamRBack)) 
        |> RealTimeDeque.check 
   
    ///O(1), worst case. Returns the first element.
    member this.Head =
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | LazyList.Nil, LazyList.Cons(x, _) -> x
        | LazyList.Cons(x, _), _ -> x

    ///O(1), worst case. Returns option first element.
    member this.TryGetHead =
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | LazyList.Nil, LazyList.Cons(x, _) -> Some(x)
        | LazyList.Cons(x, _), _ -> Some(x)

    ///O(1), worst case. Returns a new deque of the elements before the last element.
    member this.Init = 
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | _, LazyList.Nil -> RealTimeDeque.Empty this.c 
        | _, LazyList.Cons(x, xs) ->
            new RealTimeDeque<'a>(this.c, this.frontLength, this.front, (RealTimeDeque.exec2 this.streamFront), (this.rBackLength-1), xs, (RealTimeDeque.exec2 this.streamRBack))
            |> RealTimeDeque.check 

    ///O(1), worst case. Returns option deque of the elements before the last element.
    member this.TryGetInit = 
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | _, LazyList.Nil -> Some(RealTimeDeque.Empty this.c)
        | _, LazyList.Cons(x, xs) ->
            Some(new RealTimeDeque<'a>(this.c, this.frontLength, this.front, (RealTimeDeque.exec2 this.streamFront), (this.rBackLength-1), xs, (RealTimeDeque.exec2 this.streamRBack))
            |> RealTimeDeque.check) 
          
    ///O(1). Returns true if the deque has no elements.
    member this.IsEmpty =  
        ((this.frontLength = 0) && (this.rBackLength = 0))

    ///O(1), worst case. Returns the last element.
    member this.Last = 
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | _, LazyList.Cons(x, _) ->  x
        | LazyList.Cons(x, _), LazyList.Nil-> x

    ///O(1), worst case. Returns option last element.
    member this.TryGetLast = 
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | _, LazyList.Cons(x, _) ->  Some(x)
        | LazyList.Cons(x, _), LazyList.Nil-> Some(x)

    ///O(1). Returns the count of elememts.
    member this.Length = RealTimeDeque.length this

    ///O(n/2), worst case. Returns element by index.
    member this.Lookup (i:int) =
        match frontLength, front, rBackLength, rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> raise Exceptions.OutOfBounds
        | lenF, front, lenR, rear when i < lenF -> 
            let rec loopF = function 
                | xs, i'  when i' = 0 -> LazyList.head xs
                | xs, i' -> loopF ((LazyList.tail xs), (i' - 1))
            loopF (front, i)
        | lenF, front, lenR, rear ->  
            let rec loopF = function 
                | xs, i'  when i' = 0 -> LazyList.head xs
                | xs, i' -> loopF ((LazyList.tail xs), (i' - 1))
            loopF (rear, ((lenR - (i - lenF)) - 1))

    ///O(n/2), worst case. Returns option element by index.
    member this.TryLookup (i:int) =
        match frontLength, front, rBackLength, rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> None
        | lenF, front, lenR, rear when i < lenF -> 
            let rec loopF = function 
                | xs, i'  when i' = 0 -> Some(LazyList.head xs)
                | xs, i' -> loopF ((LazyList.tail xs), (i' - 1))
            loopF (front, i)
        | lenF, front, lenR, rear ->  
            let rec loopF = function 
                | xs, i'  when i' = 0 -> Some(LazyList.head xs)
                | xs, i' -> loopF ((LazyList.tail xs), (i' - 1))
            loopF (rear, ((lenR - (i - lenF)) - 1))

    ///O(n/2), worst case. Returns deque with element removed by index.
    member this.Remove (i:int) =
        match frontLength, front, rBackLength, rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> raise Exceptions.OutOfBounds
        | lenF, front, lenR, rear when i < lenF -> 
            let newFront = 
                if (i = 0) then LazyList.tail front
                else 
                    let left, right = lLsplit front i
                    LazyList.append (List.rev left |> LazyList.ofList) right

            new RealTimeDeque<'a>(c, (lenF - 1), newFront, LazyList.empty, lenR, rear, LazyList.empty)
            |> RealTimeDeque.check2

        | lenF, front, lenR, rear ->  
            let n = lenR - (i - lenF) - 1
            let newRear = 
                if (n = 0) then LazyList.tail rear
                else 
                    let left, right = lLsplit rear n
                    LazyList.append (List.rev left |> LazyList.ofList) right

            new RealTimeDeque<'a>(c, lenF, front, LazyList.empty, (lenR - 1), newRear, LazyList.empty)
            |> RealTimeDeque.check2

    ///O(n/2), worst case. Returns option deque with element removed by index.
    member this.TryRemove (i:int) =
        match frontLength, front, rBackLength, rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> None
        | lenF, front, lenR, rear when i < lenF -> 
            let newFront = 
                if (i = 0) then LazyList.tail front
                else 
                    let left, right = lLsplit front i
                    LazyList.append (List.rev left |> LazyList.ofList) right

            let z = new RealTimeDeque<'a>(c, (lenF - 1), newFront, LazyList.empty, lenR, rear, LazyList.empty) |> RealTimeDeque.check2
            Some(z)

        | lenF, front, lenR, rear ->  
            let n = lenR - (i - lenF) - 1
            let newRear = 
                if (n = 0) then LazyList.tail rear
                else 
                    let left, right = lLsplit rear n
                    LazyList.append (List.rev left |> LazyList.ofList) right
        
            let z = new RealTimeDeque<'a>(c, lenF, front, LazyList.empty, (lenR - 1), newRear, LazyList.empty) |> RealTimeDeque.check2
            Some(z)

    ///O(1). Returns deque reversed.
    member this.Rev = 
        (new RealTimeDeque<'a>(c, rBackLength, rBack, streamRBack, frontLength, front, streamFront))

    ///O(1), worst case. Returns a new deque with the element added to the end.
    member this.Snoc x = 
        new RealTimeDeque<'a>(this.c, this.frontLength, this.front, (RealTimeDeque.exec1 this.streamFront), (this.rBackLength+1), (LazyList.cons x this.rBack), (RealTimeDeque.exec1 this.streamRBack))
        |> RealTimeDeque.check

    ///O(1), worst case. Returns a new deque of the elements trailing the first element.
    member this.Tail =
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | LazyList.Nil, LazyList.Cons(x, _) -> RealTimeDeque.Empty this.c 
        | LazyList.Cons(x, xs), _ ->
            new RealTimeDeque<'a>(this.c, (this.frontLength-1), xs, (RealTimeDeque.exec2 this.streamFront), this.rBackLength, this.rBack, (RealTimeDeque.exec2 this.streamRBack))
            |> RealTimeDeque.check

    ///O(1), worst case. Returns option deque of the elements trailing the first element.
    member this.TryGetTail =
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | LazyList.Nil, LazyList.Cons(x, _) -> Some(RealTimeDeque.Empty this.c )
        | LazyList.Cons(x, xs), _ ->
            Some(new RealTimeDeque<'a>(this.c, (this.frontLength-1), xs, (RealTimeDeque.exec2 this.streamFront), this.rBackLength, this.rBack, (RealTimeDeque.exec2 this.streamRBack))
            |> RealTimeDeque.check)

    ///O(1), worst case. Returns the first element and tail
    member this.Uncons =  
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | _, _ -> this.Head, this.Tail

    ///O(1), worst case. Returns option first element and tail.
    member this.TryUncons =  
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | _, _ -> Some(this.Head, this.Tail)

    ///O(1), worst case. Returns init and the last element.
    member this.Unsnoc =  
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | _, _ -> this.Init, this.Last

    ///O(1), worst case. Returns option init and the last element.
    member this.TryUnsnoc =  
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | _, _ -> Some(this.Init, this.Last)
          
    ///O(n/2), worst case. Returns deque with element updated by index.
    member this.Update (i:int) (y: 'a) =
        match frontLength, front, rBackLength, rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> raise Exceptions.OutOfBounds
        | lenF, front, lenR, rear when i < lenF -> 
            let newFront = 
                if (i = 0) then LazyList.cons y (LazyList.tail front)
                else 
                    let left, right = lLsplit front i
                    LazyList.append (List.rev left |> LazyList.ofList) (LazyList.cons y right)

            new RealTimeDeque<'a>(c, lenF, newFront, LazyList.empty, lenR, rear, LazyList.empty)
            |> RealTimeDeque.check2

        | lenF, front, lenR, rear ->  
            let n = lenR - (i - lenF) - 1
            let newRear = 
                if (n = 0) then LazyList.cons y (LazyList.tail rear)
                else 
                    let left, right = lLsplit rear n
                    LazyList.append (List.rev left |> LazyList.ofList) (LazyList.cons y right)
        
            new RealTimeDeque<'a>(c, lenF, front, LazyList.empty, lenR, newRear, LazyList.empty)
            |> RealTimeDeque.check2

    ///O(n/2), worst case. Returns option deque with element updated by index.
    member this.TryUpdate (i:int) (y: 'a) =
        match frontLength, front, rBackLength, rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> None
        | lenF, front, lenR, rear when i < lenF -> 
            let newFront = 
                if (i = 0) then LazyList.cons y (LazyList.tail front)
                else 
                    let left, right = lLsplit front i
                    LazyList.append (List.rev left |> LazyList.ofList) (LazyList.cons y right)

            let z = new RealTimeDeque<'a>(c, lenF, newFront, LazyList.empty, lenR, rear, LazyList.empty) |> RealTimeDeque.check2
            Some(z)

        | lenF, front, lenR, rear ->  
            let n = lenR - (i - lenF) - 1
            let newRear = 
                if (n = 0) then LazyList.cons y (LazyList.tail rear)
                else 
                    let left, right = lLsplit rear n
                    LazyList.append (List.rev left |> LazyList.ofList) (LazyList.cons y right)
        
            let z = new RealTimeDeque<'a>(c, lenF, front, LazyList.empty, lenR, newRear, LazyList.empty) |> RealTimeDeque.check2
            Some(z)
    with
    interface IDeque<'a> with

        member this.Cons x = this.Cons x :> _

        member this.Count = this.Length

        member this.Head = this.Head

        member this.TryGetHead = this.TryGetHead

        member this.Init = this.Init :> _

        member this.TryGetInit = Some(this.TryGetInit.Value :> _)

        member this.IsEmpty = this.IsEmpty

        member this.Last = this.Last

        member this.TryGetLast = this.TryGetLast

        member this.Length = this.Length

        member this.Lookup i = this.Lookup i

        member this.TryLookup i = this.TryLookup i

        member this.Remove i = this.Remove i :> _

        member this.TryRemove i = 
            match this.TryRemove i with
            | None -> None
            | Some(q) -> Some(q :> _)

        member this.Rev = this.Rev :> _

        member this.Snoc x = this.Snoc x :> _

        member this.Tail = this.Tail :> _

        member this.TryGetTail =
            match this.TryGetTail with
            | None -> None
            | Some(q) -> Some(q :> _)

        member this.Uncons = 
            let x, xs = this.Uncons 
            x, xs :> _

        member this.TryUncons = 
            match this.TryUncons with
            | None -> None
            | Some(x, q) -> Some(x, q :> _)

        member this.Unsnoc = 
            let xs, x = this.Unsnoc 
            xs :> _, x

        member this.TryUnsnoc = 
            match this.TryUnsnoc with
            | None -> None
            | Some(q, x) -> Some(q :> _, x)

        member this.Update i y  = this.Update i y :> _

        member this.TryUpdate i y  =
            match this.TryUpdate i y with
            | None -> None
            | Some(q) -> Some(q :> _)

    interface IEnumerable<'a> with

        member this.GetEnumerator() = 
            let e = seq {
                  yield! front
                  yield! (lLrev this.rBack)  }
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

module RealTimeDeque =
    //pattern discriminators

    let (|Cons|Nil|) (q : RealTimeDeque<'a>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    let (|Snoc|Nil|) (q : RealTimeDeque<'a>) = match q.TryUnsnoc with Some(a,b) -> Snoc(a,b) | None -> Nil

    let private stndC = 2

    ///O(|ys-xs|). Returns a deque of the two deques concatenated, front-back stream ratio constant defaulted to 2.
    let append (xs : RealTimeDeque<'a>) (ys : RealTimeDeque<'a>) = RealTimeDeque.AppendC stndC xs ys

    ///O(|ys-xs|). Returns a deque of the two deques concatenated, c is front-back stream ratio constant, should be at least 2.
    let appendC c (xs : RealTimeDeque<'a>) (ys : RealTimeDeque<'a>) = RealTimeDeque.AppendC c xs ys

    ///O(1), worst case. Returns a new deque with the element added to the beginning.
    let inline cons (x : 'a) (q : RealTimeDeque<'a>) = q.Cons x 

    ///O(1). Returns deque of no elements, c is front-back stream ration constant, should be at least 2.
    let empty c = RealTimeDeque.Empty c

    ///O(1), worst case. Returns the first element.
    let inline head (q : RealTimeDeque<'a>) = q.Head

    ///O(1), worst case. Returns option first element.
    let inline tryGetHead (q : RealTimeDeque<'a>) = q.TryGetHead

    ///O(1), worst case. Returns a new deque of the elements before the last element.
    let inline init (q : RealTimeDeque<'a>) = q.Init 

    ///O(1), worst case. Returns option deque of the elements before the last element.
    let inline tryGetInit (q : RealTimeDeque<'a>) = q.TryGetInit 

    ///O(1). Returns true if the deque has no elements.
    let inline isEmpty (q : RealTimeDeque<'a>) = q.IsEmpty

    ///O(1), worst case. Returns the last element.
    let inline last (q : RealTimeDeque<'a>) = q.Last

    ///O(1), worst case. Returns option last element.
    let inline tryGetLast (q : RealTimeDeque<'a>) = q.TryGetLast

    ///O(1). Returns the count of elememts.
    let inline length (q : RealTimeDeque<'a>) = q.Length

    ///O(n/2), worst case. Returns option element by index.
    let inline lookup i (q : RealTimeDeque<'a>) = q.Lookup i

    ///O(n/2), worst case. Returns option element by index.
    let inline tryLookup i (q : RealTimeDeque<'a>) = q.TryLookup i

    ///O(|ys-xs|). Returns a deque of the two lists concatenated, front-back stream ratio constant defaulted to 2.
    let ofCatLists xs ys = RealTimeDeque.OfCatListsC stndC xs ys

    ///O(|ys-xs|). Returns a deque of the two lists concatenated, c is front-back stream ration constant, should be at least 2.
    let ofCatListsC c xs ys = RealTimeDeque.OfCatListsC c xs ys

    ///O(|ys-xs|). Returns a deque of the two seqs concatenated, front-back stream ratio constant defaulted to 2.
    let ofCatSeqs xs ys = RealTimeDeque.OfCatSeqsC stndC xs ys

    ///O(|ys-xs|). Returns a deque of the two seqs concatenated, c is front-back stream ratio constant, should be at least 2.
    let ofCatSeqsC c xs ys = RealTimeDeque.OfCatSeqsC c xs ys

    ///O(n). Returns a deque of the seq, front-back stream ratio constant defaulted to 2.
    let ofSeq xs = RealTimeDeque.OfSeqC stndC xs

    ///O(n). Returns a deque of the seq, c is front-back stream ratio constant, should be at least 2.
    let ofSeqC c xs = RealTimeDeque.OfSeqC c xs

    ///O(n/2), worst case. Returns deque with element removed by index.
    let inline remove i (q : RealTimeDeque<'a>) = q.Remove i

    ///O(n/2), worst case. Returns option deque with element removed by index.
    let inline tryRemove i (q : RealTimeDeque<'a>) = q.TryRemove i

    ///O(1). Returns deque reversed.
    let inline rev (q : RealTimeDeque<'a>) = q.Rev

    ///O(1). Returns a deque of one element, front-back stream ratio constant defaulted to 2.
    let singleton x = empty stndC |> cons x  

    ///O(1). Returns a deque of one element, c is front-back stream ratio constant, should be at least 2.
    let singletonC c x = empty c |> cons x  

    ///O(1), worst case. Returns a new deque with the element added to the end.
    let inline snoc (x : 'a) (q : RealTimeDeque<'a>) = (q.Snoc x) 

    ///O(1), worst case. Returns a new deque of the elements trailing the first element.
    let inline tail (q : RealTimeDeque<'a>) = q.Tail 

    ///O(1), worst case. Returns option deque of the elements trailing the first element.
    let inline tryGetTail (q : RealTimeDeque<'a>) = q.TryGetTail 

    ///O(1), worst case. Returns the first element and tail.
    let inline uncons (q : RealTimeDeque<'a>) = q.Uncons

    ///O(1), worst case. Returns option first element and tail.
    let inline tryUncons (q : RealTimeDeque<'a>) = q.TryUncons

    ///O(1), worst case. Returns init and the last element.
    let inline unsnoc (q : RealTimeDeque<'a>) = q.Unsnoc

    ///O(1), worst case. Returns option init and the last element.
    let inline tryUnsnoc (q : RealTimeDeque<'a>) = q.TryUnsnoc

    ///O(n/2), worst case. Returns deque with element updated by index.
    let inline update i y (q : RealTimeDeque<'a>) = q.Update i y

    ///O(n/2), worst case. Returns option deque with element updated by index.
    let inline tryUpdate i y (q : RealTimeDeque<'a>) = q.TryUpdate i y
