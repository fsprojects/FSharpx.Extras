//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/10/batched-queue

//jf -- added ofSeq and try...
//pattern discriminators Snoc and Nil

namespace FSharpx.Collections.Experimental

open System.Collections
open System.Collections.Generic

type BatchedQueue<'T> (front : list<'T>, rBack : list<'T>) = 

    member internal this.front = front

    member internal this.rBack = rBack

    static member private checkf (q : BatchedQueue<'T>) =
        match q.front, q.rBack with
        | [], r -> BatchedQueue((List.rev r), [])
        | f, r -> BatchedQueue(f, r)

    static member private length (q : BatchedQueue<'T>) = q.front.Length + q.rBack.Length

    static member internal Empty : BatchedQueue<'T> = BatchedQueue<'T>([], []) 

    static member internal fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : BatchedQueue<'T>)  :  'State = 
        let s = List.fold f state q.front
        List.fold f s (List.rev q.rBack)

    static member internal foldBack (f : ('T -> 'State -> 'State)) (q : BatchedQueue<'T>) (state : 'State) :  'State = 
        let s = List.foldBack f (List.rev q.rBack) state 
        (List.foldBack f q.front s)

    static member internal OfList (xs:list<'T>) = 
        BatchedQueue<'T>(xs, [])

    static member internal OfSeq (xs:seq<'T>) = 
        BatchedQueue<'T>((List.ofSeq xs), [])

    ///returns the first element
    member this.Head = 
        match front with
        | hd::_ -> hd
        | _ -> raise Exceptions.Empty

    ///returns option first element
    member this.TryGetHead =
        match front with
        | hd::_ -> Some(hd)
        | _ -> None
         
    ///returns true if the queue has no elements
    member this.IsEmpty = front.IsEmpty

    ///returns the count of elememts
    member this.Length = BatchedQueue.length this

    ///returns queue reversed
    member this.Rev = 
        BatchedQueue<'T>(rBack, front) |> BatchedQueue.checkf

    ///returns a new queue with the element added to the end
    member this.Snoc x = 
        BatchedQueue<'T>(front, x::rBack)
        |> BatchedQueue.checkf

    ///returns a new queue of the elements trailing the first element
    member this.Tail =
        match front with
        | hd::tl -> 
            BatchedQueue<'T>(tl, rBack)
            |> BatchedQueue.checkf
        | _ -> raise Exceptions.Empty
            
    ///returns option queue of the elements trailing the first element
    member this.TryGetTail =
        match front with
        | hd::tl -> Some(BatchedQueue<'T>(tl, rBack) |> BatchedQueue.checkf)
        | _ -> None

    ///returns the first element and tail
    member this.Uncons =  
        match front with
        | hd::tl -> hd, (BatchedQueue<'T>(tl, rBack) |> BatchedQueue.checkf)
        | _ -> raise Exceptions.Empty

    ///returns option first element and tail
    member this.TryUncons =  
        match front with
        | hd::tl -> Some(hd, (BatchedQueue<'T>(tl, rBack) |> BatchedQueue.checkf))
        | _ -> None

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
                  yield! (List.rev rBack)}
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BatchedQueue =
    //pattern discriminators
    let (|Cons|Nil|) (q : BatchedQueue<'T>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    ///returns queue of no elements
    ///c is front-back stream ration constant, should be at least 2
    let empty() : BatchedQueue<'T> = BatchedQueue.Empty

    ///applies a function to each element of the queue, threading an accumulator argument through the computation, left to right
    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : BatchedQueue<'T>) = BatchedQueue<_>.fold f state q

    ///applies a function to each element of the queue, threading an accumulator argument through the computation, right to left
    let foldBack (f : ('T -> 'State -> 'State)) (q : BatchedQueue<'T>) (state : 'State) =  BatchedQueue<_>.foldBack f q state

    ///returns the first element
    let inline head (q : BatchedQueue<'T>) = q.Head

    ///returns option first element
    let inline tryGetHead (q : BatchedQueue<'T>) = q.TryGetHead

    ///returns true if the queue has no elements
    let inline isEmpty (q : BatchedQueue<'T>) = q.IsEmpty

    ///returns the count of elememts
    let inline length (q : BatchedQueue<'T>) = q.Length

    ///returns a queue of the list
    let ofList xs = BatchedQueue.OfList xs

    ///returns a queue of the seq
    let ofSeq xs = BatchedQueue.OfSeq xs

    ///returns queue reversed
    let inline rev (q : BatchedQueue<'T>) = q.Rev

    ///returns a new queue with the element added to the end
    let inline snoc (x : 'T) (q : BatchedQueue<'T>) = (q.Snoc x) 

    ///returns a new queue of the elements trailing the first element
    let inline tail (q : BatchedQueue<'T>) = q.Tail 

    ///returns option queue of the elements trailing the first element
    let inline tryGetTail (q : BatchedQueue<'T>) = q.TryGetTail 

    ///returns the first element and tail
    let inline uncons (q : BatchedQueue<'T>) = q.Uncons

    ///returns option first element and tail
    let inline tryUncons (q : BatchedQueue<'T>) = q.TryUncons