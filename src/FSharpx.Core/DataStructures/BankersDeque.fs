//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/11/bankers-double-ended-queue

//jf -- added rev, ofSeq, ofSeqC, lookup, append, appendC, update, remove, tryUpdate, tryRemove
//   -- added standardized C of 2 for singleton, empty, and ofSeq based on my reading of Okasaki

//pattern discriminators Cons, Snoc, and Nil

namespace FSharpx.DataStructures

open FSharpx.Collections
open LazyListHelpr
open System.Collections
open System.Collections.Generic

type BankersDeque<'a> (c : int, frontLength : int, front : LazyList<'a>,  rBackLength : int, rBack : LazyList<'a>) = 

    member private this.c = c

    member private this.frontLength = frontLength

    member private this.front = front

    member private this.rBackLength = rBackLength

    member private this.rBack = rBack

    static member private length (q : BankersDeque<'a>) = q.frontLength + q.rBackLength

    static member private check (q : BankersDeque<'a>) =
        let n = BankersDeque.length q
        if q.frontLength > q.c * q.rBackLength + 1 then
            let i= n / 2
            let j = n - i
            let f' = LazyList.take i q.front
            let r' = lLdrop i q.front |> lLrev |> LazyList.append q.rBack
            new BankersDeque<'a>(q.c, i, f', j, r')
        elif q.rBackLength > q.c * q.frontLength + 1 then
            let j = n / 2
            let i = n - j
            let r' = LazyList.take j q.rBack
            let f' = lLdrop j q.rBack |> lLrev |> LazyList.append q.front
            new BankersDeque<'a>(q.c, i, f', j, r')
        else
            q

    static member internal AppendC cC (xs:BankersDeque<'a>) (ys:BankersDeque<'a>) =
        let front2 = xs.frontLength + xs.rBackLength
        let front3 = xs.frontLength + xs.rBackLength + ys.frontLength
        let back2 = ys.frontLength + ys.rBackLength
        let back3 = xs.rBackLength + ys.frontLength + ys.rBackLength
        match (front2, front3, back2, back3) with 
        | a, b, c, d when (abs(xs.frontLength - d) <= abs(a - c)) && (abs(xs.frontLength - d) <= abs(a - ys.rBackLength) && (xs.frontLength > 0)) -> 
            new BankersDeque<'a>(cC, xs.frontLength, xs.front, (xs.rBackLength + ys.frontLength + ys.rBackLength), (LazyList.append ys.rBack (LazyList.append  (lLrev ys.front) xs.rBack)))
            |> BankersDeque.check
        | a, b, c, d when (abs(a - c) <= abs(xs.frontLength - d)) && (abs(a - c) <= abs(a - ys.rBackLength)) -> 
            new BankersDeque<'a>(cC, (xs.frontLength + xs.rBackLength), (LazyList.append xs.front (lLrev xs.rBack)), (ys.frontLength + ys.rBackLength), (LazyList.append ys.rBack (lLrev ys.front)))
            |> BankersDeque.check
        | a, b, c, d ->
            new BankersDeque<'a>(cC, (xs.frontLength + xs.rBackLength + ys.frontLength), (LazyList.append (LazyList.append xs.front (lLrev xs.rBack)) ys.front), ys.rBackLength, ys.rBack)
            |> BankersDeque.check

    static member internal Empty c = new BankersDeque<'a>(c, 0, (LazyList.empty), 0, (LazyList.empty)) 

    static member internal OfCatListsC c (xs : 'a list) (ys : 'a list) =
        new BankersDeque<'a>(c, xs.Length, (LazyList.ofList xs), ys.Length, (LazyList.ofList (List.rev ys)))
        |> BankersDeque.check

    static member internal OfCatSeqsC c (xs : 'a seq) (ys : 'a seq) =
        new BankersDeque<'a>(c, (Seq.length xs), (LazyList.ofSeq xs), (Seq.length ys), (lLrev (LazyList.ofSeq ys)))
        |> BankersDeque.check

    static member internal OfSeqC c (xs:seq<'a>) = 
        new BankersDeque<'a>(c, (Seq.length xs), (LazyList.ofSeq xs), 0, (LazyList.empty))
        |> BankersDeque.check

    ///O(1), amortized. Returns a new deque with the element added to the beginning.
    member this.Cons x =
        new BankersDeque<'a>(this.c, (this.frontLength+1), (LazyList.cons x this.front), this.rBackLength, this.rBack) 
        |> BankersDeque.check 
   
    ///O(1), amortized. Returns the first element.
    member this.Head =
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | LazyList.Nil, LazyList.Cons(x, _) -> x
        | LazyList.Cons(x, _), _ -> x

    ///O(1), amortized. Returns option first element.
    member this.TryGetHead =
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | LazyList.Nil, LazyList.Cons(x, _) -> Some(x)
        | LazyList.Cons(x, _), _ -> Some(x)

    ///O(1), amortized. Returns a new deque of the elements before the last element.
    member this.Init = 
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | _, LazyList.Nil -> BankersDeque.Empty this.c 
        | _, LazyList.Cons(x, xs) ->
            new BankersDeque<'a>(this.c, this.frontLength, this.front, (this.rBackLength-1), xs)
            |> BankersDeque.check 

    ///O(1), amortized. Returns option deque of the elements before the last element.
    member this.TryGetInit = 
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | _, LazyList.Nil -> Some(BankersDeque.Empty this.c)
        | _, LazyList.Cons(x, xs) ->
            Some(new BankersDeque<'a>(this.c, this.frontLength, this.front, (this.rBackLength-1), xs) |> BankersDeque.check)
         
    ///O(1). Returns true if the deque has no elements.
    member this.IsEmpty =  
        ((this.frontLength = 0) && (this.rBackLength = 0))

    ///O(1), amortized. Returns the last element.
    member this.Last = 
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | _, LazyList.Cons(x, _) ->  x
        | LazyList.Cons(x, _), LazyList.Nil-> x

    ///O(1), amortized. Returns option last element.
    member this.TryGetLast = 
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | _, LazyList.Cons(x, _) -> Some(x)
        | LazyList.Cons(x, _), LazyList.Nil-> Some(x)

    ///O(1). Returns the count of elememts.
    member this.Length = BankersDeque.length this

    ///O(n), worst case. Returns element by index.
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

    ///O(n), worst case. Returns option element by index.
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

    ///O(n), worst case. Returns deque with element removed by index.
    member this.Remove (i:int) =
        match frontLength, front, rBackLength, rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> raise Exceptions.OutOfBounds
        | lenF, front, lenR, rear when i < lenF -> 
            let newFront = 
                if (i = 0) then LazyList.tail front
                else 
                    let left, right = lLsplit front i
                    LazyList.append (List.rev left |> LazyList.ofList) right

            new BankersDeque<'a>(c, (lenF - 1), newFront, lenR, rear)
            |> BankersDeque.check

        | lenF, front, lenR, rear ->  
            let n = lenR - (i - lenF) - 1
            let newRear = 
                if (n = 0) then LazyList.tail rear
                else 
                    let left, right = lLsplit rear n
                    LazyList.append (List.rev left |> LazyList.ofList) right

            new BankersDeque<'a>(c, lenF, front, (lenR - 1), newRear)
            |> BankersDeque.check

    ///O(n), worst case. Returns option deque with element removed by index.
    member this.TryRemove (i:int) =
        match frontLength, front, rBackLength, rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> None
        | lenF, front, lenR, rear when i < lenF -> 
            let newFront = 
                if (i = 0) then LazyList.tail front
                else 
                    let left, right = lLsplit front i
                    LazyList.append (List.rev left |> LazyList.ofList) right

            let z = new BankersDeque<'a>(c, (lenF - 1), newFront, lenR, rear) |> BankersDeque.check
            Some(z)

        | lenF, front, lenR, rear ->  
            let n = lenR - (i - lenF) - 1
            let newRear = 
                if (n = 0) then LazyList.tail rear
                else 
                    let left, right = lLsplit rear n
                    LazyList.append (List.rev left |> LazyList.ofList) right
        
            let z = new BankersDeque<'a>(c, lenF, front, (lenR - 1), newRear) |> BankersDeque.check
            Some(z)

    ///O(1). Returns deque reversed.
    member this.Rev = 
        (new BankersDeque<'a>(c, rBackLength, rBack, frontLength, front))

    ///O(1), amortized. Returns a new deque with the element added to the end.
    member this.Snoc x = 
        new BankersDeque<'a>(this.c, this.frontLength, this.front, (this.rBackLength + 1), (LazyList.cons x this.rBack))
        |> BankersDeque.check

    ///O(1), amortized. Returns a new deque of the elements trailing the first element.
    member this.Tail =
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | LazyList.Nil, LazyList.Cons(x, _) -> BankersDeque.Empty this.c 
        | LazyList.Cons(x, xs), _ ->
            new BankersDeque<'a>(this.c, (this.frontLength-1), xs, this.rBackLength, this.rBack)
            |> BankersDeque.check

    ///O(1), amortized. Returns option deque of the elements trailing the first element.
    member this.TryGetTail =
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | LazyList.Nil, LazyList.Cons(x, _) -> Some(BankersDeque.Empty this.c)
        | LazyList.Cons(x, xs), _ ->
            Some(new BankersDeque<'a>(this.c, (this.frontLength-1), xs, this.rBackLength, this.rBack)
            |> BankersDeque.check)

    ///O(1), amortized. Returns the first element and tail.
    member this.Uncons =  
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | _, _ -> this.Head, this.Tail

    ///O(1), amortized. Returns option first element and tail.
    member this.TryUncons =  
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | _, _ -> Some(this.Head, this.Tail)

    ///O(1), amortized. Returns init and the last element.
    member this.Unsnoc =  
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> raise Exceptions.Empty
        | _, _ -> this.Init, this.Last

    ///O(1), amortized. Returns option init and the last element.
    member this.TryUnsnoc =  
        match this.front, this.rBack with
        | LazyList.Nil, LazyList.Nil -> None
        | _, _ -> Some(this.Init, this.Last)

    ///O(n), worst case. Returns deque with element updated by index.
    member this.Update (i:int) (y: 'a) =
        match this.frontLength, this.front, this.rBackLength, this.rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> raise Exceptions.OutOfBounds
        | lenF, front, lenR, rear when i < lenF -> 
            let newFront = 
                if (i = 0) then LazyList.cons y (LazyList.tail front)
                else 
                    let left, right = lLsplit front i
                    LazyList.append (List.rev left |> LazyList.ofList) (LazyList.cons y right)

            new BankersDeque<'a>(c, lenF, newFront, lenR, rear)
            |> BankersDeque.check

        | lenF, front, lenR, rear ->  
            let n = lenR - (i - lenF) - 1
            let newRear = 
                if (n = 0) then LazyList.cons y (LazyList.tail rear)
                else 
                    let left, right = lLsplit rear n
                    LazyList.append (List.rev left |> LazyList.ofList) (LazyList.cons y right)
        
            new BankersDeque<'a>(c, lenF, front, lenR, newRear)
            |> BankersDeque.check

    ///O(n), worst case. Returns option deque with element updated by index.
    member this.TryUpdate (i:int) (y: 'a) =
        match frontLength, front, rBackLength, rBack with
        | lenF, front, lenR, rear when i > (lenF + lenR - 1) -> None
        | lenF, front, lenR, rear when i < lenF -> 
            let newFront = 
                if (i = 0) then LazyList.cons y (LazyList.tail front)
                else 
                    let left, right = lLsplit front i
                    LazyList.append (List.rev left |> LazyList.ofList) (LazyList.cons y right)

            let z = new BankersDeque<'a>(c, lenF, newFront, lenR, rear) |> BankersDeque.check 
            Some(z)

        | lenF, front, lenR, rear ->  
            let n = lenR - (i - lenF) - 1
            let newRear = 
                if (n = 0) then LazyList.cons y (LazyList.tail rear)
                else 
                    let left, right = lLsplit rear n
                    LazyList.append (List.rev left |> LazyList.ofList) (LazyList.cons y right)
        
            let z = new BankersDeque<'a>(c, lenF, front, lenR, newRear) |> BankersDeque.check
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BankersDeque =
    //pattern discriminators

    let (|Cons|Nil|) (q : BankersDeque<'a>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    let (|Snoc|Nil|) (q : BankersDeque<'a>) = match q.TryUnsnoc with Some(a,b) -> Snoc(a,b) | None -> Nil

    let private stndC = 2

    ///O(ys-xs). Returns a deque of the two deques concatenated, front-back stream ratio constant defaulted to 2.
    let append (xs : BankersDeque<'a>) (ys : BankersDeque<'a>) = BankersDeque.AppendC stndC xs ys

    ///O(ys-xs). Returns a deque of the two deques concatenated, c is front-back stream ratio constant, should be at least 2.
    let appendC c (xs : BankersDeque<'a>) (ys : BankersDeque<'a>) = BankersDeque.AppendC c xs ys

    ///O(1), amortized. Returns a new deque with the element added to the beginning.
    let inline cons (x : 'a) (q : BankersDeque<'a>) = q.Cons x 

    ///O(1). Returns deque of no elements, c is front-back stream ration constant, should be at least 2.
    let empty c = BankersDeque.Empty c

    ///O(1), amortized. Returns the first element.
    let inline head (q : BankersDeque<'a>) = q.Head

    ///O(1), amortized. Returns option first element.
    let inline tryGetHead (q : BankersDeque<'a>) = q.TryGetHead

    ///O(1), amortized. Returns a new deque of the elements before the last element.
    let inline init (q : BankersDeque<'a>) = q.Init 

    ///O(1), amortized. Returns option deque of the elements before the last element.
    let inline tryGetInit (q : BankersDeque<'a>) = q.TryGetInit 

    ///O(1). Returns true if the deque has no elements.
    let inline isEmpty (q : BankersDeque<'a>) = q.IsEmpty

    ///O(1), amortized. Returns the last element.
    let inline last (q : BankersDeque<'a>) = q.Last

    ///O(1), amortized. Returns option last element.
    let inline tryGetLast (q : BankersDeque<'a>) = q.TryGetLast

    ///O(1). Returns the count of elememts.
    let inline length (q : BankersDeque<'a>) = q.Length

    ///O(n), worst case. Returns element by index.
    let inline lookup i (q : BankersDeque<'a>) = q.Lookup i

    ///O(n), worst case. Returns option element by index.
    let inline tryLookup i (q : BankersDeque<'a>) = q.TryLookup i

    ///O(ys-xs). Returns a deque of the two lists concatenated, front-back stream ratio constant defaulted to 2.
    let ofCatLists xs ys = BankersDeque.OfCatListsC stndC xs ys

    ///O(ys-xs). Returns a deque of the two lists concatenated, c is front-back stream ration constant, should be at least 2.
    let ofCatListsC c xs ys = BankersDeque.OfCatListsC c xs ys

    ///O(ys). Returns a deque of the two seqs concatenated, front-back stream ratio constant defaulted to 2.
    let ofCatSeqs xs ys = BankersDeque.OfCatSeqsC stndC xs ys

    ///O(ys). Returns a deque of the two seqs concatenated, c is front-back stream ratio constant, should be at least 2.
    let ofCatSeqsC c xs ys = BankersDeque.OfCatSeqsC c xs ys

    ///O(1). Returns a deque of the seq, front-back stream ratio constant defaulted to 2.
    let ofSeq xs = BankersDeque.OfSeqC stndC xs

    ///O(1). Returns a deque of the seq, c is front-back stream ratio constant, should be at least 2.
    let ofSeqC c xs = BankersDeque.OfSeqC c xs

    ///O(n), worst case. Returns deque with element removed by index.
    let inline remove i (q : BankersDeque<'a>) = q.Remove i

    ///O(n), worst case. Returns option deque with element removed by index.
    let inline tryRemove i (q : BankersDeque<'a>) = q.TryRemove i

    ///O(1). Returns deque reversed.
    let inline rev (q : BankersDeque<'a>) = q.Rev

    ///O(1). Returns a deque of one element, front-back stream ratio constant defaulted to 2.
    let singleton x = empty stndC |> cons x  

    ///O(1). Returns a deque of one element, c is front-back stream ratio constant, should be at least 2.
    let singletonC c x = empty c |> cons x  

    ///O(1), amortized. Returns a new deque with the element added to the end.
    let inline snoc (x : 'a) (q : BankersDeque<'a>) = (q.Snoc x) 

    ///O(1), amortized. Returns a new deque of the elements trailing the first element.
    let inline tail (q : BankersDeque<'a>) = q.Tail 

    ///O(1), amortized. Returns option deque of the elements trailing the first element.
    let inline tryGetTail (q : BankersDeque<'a>) = q.TryGetTail 

    ///O(1), amortized. Returns the first element and tail.
    let inline uncons (q : BankersDeque<'a>) = q.Uncons

    ///O(1), amortized. Returns option first element and tail.
    let inline tryUncons (q : BankersDeque<'a>) = q.TryUncons

    ///O(1), amortized. Returns init and the last element.
    let inline unsnoc (q : BankersDeque<'a>) = q.Unsnoc

    ///O(1), amortized. Returns option init and the last element.
    let inline tryUnsnoc (q : BankersDeque<'a>) = q.TryUnsnoc

    ///O(n), worst case. Returns deque with element updated by index.
    let inline update i y (q : BankersDeque<'a>) = q.Update i y

    ///O(n), worst case. Returns option deque with element updated by index.
    let inline tryUpdate i y (q : BankersDeque<'a>) = q.TryUpdate i y
