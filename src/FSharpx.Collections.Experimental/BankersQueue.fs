//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/31/bankers-queue

//jf -- added ofSeq and try...
//pattern discriminators Snoc and Nil

namespace FSharpx.Collections.Experimental

open FSharpx.Collections
open System.Collections
open System.Collections.Generic

type BankersQueue<'T> (frontLength : int, front : LazyList<'T>, backLength : int, back : LazyList<'T>) = 

    member private this.frontLength = frontLength

    member internal this.front = front

    member private this.backLength = backLength

    member internal this.back = back

    static member private check (q : BankersQueue<'T>) =
        if q.backLength <= q.frontLength
        then q
        else BankersQueue((q.backLength + q.frontLength), (LazyList.append q.front (LazyList.rev q.back)), 0, LazyList.empty)

    static member private length (q : BankersQueue<'T>) = q.frontLength + q.backLength

    static member internal Empty() = new BankersQueue<'T>(0, LazyList.empty, 0, LazyList.empty) 

    static member internal OfSeq (xs:seq<'T>) = 
        BankersQueue<'T>((Seq.length xs), (LazyList.ofSeq xs), 0, LazyList.empty)
   
    ///O(1), amortized. Returns the first element.
    member this.Head =
        if (this.frontLength = 0)  
        then raise Exceptions.Empty
        else LazyList.head front

    ///O(1), amortized. Returns option first element.
    member this.TryGetHead =
        if (this.frontLength = 0)  then None
        else Some(LazyList.head front)
         
    ///O(1). Returns true if the queue has no elements.
    member this.IsEmpty = (frontLength = 0)

    ///O(1). Returns the count of elememts.
    member this.Length = BankersQueue.length this

    ///O(1). Returns queue reversed
    member this.Rev = BankersQueue<'T>(backLength, back, frontLength, front) |> BankersQueue.check

    ///O(1), amortized. Returns a new queue with the element added to the end.
    member this.Snoc x = 
        BankersQueue<'T>(frontLength, front, (backLength + 1), (LazyList.cons x back))
        |> BankersQueue.check

    ///O(1), amortized. Returns a new queue of the elements trailing the first element.
    member this.Tail =
        if (this.frontLength = 0)  then raise Exceptions.Empty
        else 
            BankersQueue<'T>((frontLength-1), (LazyList.tail front), backLength, back)
            |> BankersQueue.check

    ///O(1), amortized. Returns option queue of the elements trailing the first element.
    member this.TryGetTail =
        if (this.frontLength = 0)  then None
        else 
            Some(BankersQueue<'T>((frontLength-1), (LazyList.tail front), backLength, back)
            |> BankersQueue.check)

    ///O(1), amortized. Returns the first element and tail.
    member this.Uncons =  
        if (this.frontLength = 0)  then raise Exceptions.Empty
        else (LazyList.head front), (BankersQueue<'T>((frontLength - 1), (LazyList.tail front), backLength, back) |> BankersQueue.check)

    ///O(1), amortized. Returns option first element and tail.
    member this.TryUncons =  
        if (this.frontLength = 0)  then None
        else Some((LazyList.head front), (BankersQueue<'T>((frontLength-1), (LazyList.tail front), backLength, back) |> BankersQueue.check))

    interface IQueue<'T> with

        member this.Count() = this.Length

        member this.Head = this.Head

        member this.TryGetHead = this.TryGetHead

        member this.IsEmpty = this.IsEmpty

        member this.Length() = this.Length

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
          
    interface IEnumerable<'T> with

        member this.GetEnumerator() = 
            let e = seq {
                  yield! front
                  yield! (LazyList.rev back)  }
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BankersQueue =
    //pattern discriminators
    let (|Cons|Nil|) (q : BankersQueue<'T>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    ///O(1). Returns queue of no elements.
    let empty() = BankersQueue.Empty()

    ///O(1), amortized. Returns the first element.
    let inline head (q : BankersQueue<'T>) = q.Head

    ///O(1), amortized. Returns option first element.
    let inline tryGetHead (q : BankersQueue<'T>) = q.TryGetHead

    ///O(1). Returns true if the queue has no elements.
    let inline isEmpty() (q : BankersQueue<'T>) = q.IsEmpty

    ///O(1). Returns the count of elememts.
    let inline length() (q : BankersQueue<'T>) = q.Length

    ///O(1). Returns a queue of the seq.
    let ofSeq xs = BankersQueue.OfSeq xs

    ///O(1). Returns queue reversed.
    let inline rev (q : BankersQueue<'T>) = q.Rev

    ///O(1), amortized. Returns a new queue with the element added to the end.
    let inline snoc (x : 'T) (q : BankersQueue<'T>) = (q.Snoc x) 

    ///O(1), amortized. Returns a new queue of the elements trailing the first element.
    let inline tail (q : BankersQueue<'T>) = q.Tail 

    ///O(1), amortized. Returns option queue of the elements trailing the first element.
    let inline tryGetTail (q : BankersQueue<'T>) = q.TryGetTail 

    ///O(1), amortized. Returns the first element and tail.
    let inline uncons (q : BankersQueue<'T>) = q.Uncons

    ///O(1), amortized. Returns option first element and tail.
    let inline tryUncons (q : BankersQueue<'T>) = q.TryUncons
