//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/10/batched-queue

//jf -- added ofSeq and try...
//pattern discriminators Snoc and Nil

namespace FSharpx.DataStructures

#nowarn "44"
open System.Collections
open System.Collections.Generic

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
type BatchedQueue<'a> (front : list<'a>, rBack : list<'a>) = 

    member internal this.front = front

    member internal this.rBack = rBack

    static member private checkf (q : BatchedQueue<'a>) =
        match q.front, q.rBack with
        | [], r -> BatchedQueue((List.rev r), [])
        | f, r -> BatchedQueue(f, r)

    static member private length (q : BatchedQueue<'a>) = q.front.Length + q.rBack.Length

    static member internal Empty : BatchedQueue<'a> = BatchedQueue<'a>([], []) 

    static member internal fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : BatchedQueue<'T>)  :  'State = 
        let s = List.fold f state q.front
        List.fold f s (List.rev q.rBack)

    static member internal foldBack (f : ('T -> 'State -> 'State)) (q : BatchedQueue<'T>) (state : 'State) :  'State = 
        let s = List.foldBack f (List.rev q.rBack) state 
        (List.foldBack f q.front s)

    static member internal OfList (xs:list<'a>) = 
        BatchedQueue<'a>(xs, [])

    static member internal OfSeq (xs:seq<'a>) = 
        BatchedQueue<'a>((List.ofSeq xs), [])

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
        BatchedQueue<'a>(rBack, front) |> BatchedQueue.checkf

    ///returns a new queue with the element added to the end
    member this.Snoc x = 
        BatchedQueue<'a>(front, x::rBack)
        |> BatchedQueue.checkf

    ///returns a new queue of the elements trailing the first element
    member this.Tail =
        match front with
        | hd::tl -> 
            BatchedQueue<'a>(tl, rBack)
            |> BatchedQueue.checkf
        | _ -> raise Exceptions.Empty
            
    ///returns option queue of the elements trailing the first element
    member this.TryGetTail =
        match front with
        | hd::tl -> Some(BatchedQueue<'a>(tl, rBack) |> BatchedQueue.checkf)
        | _ -> None

    ///returns the first element and tail
    member this.Uncons =  
        match front with
        | hd::tl -> hd, (BatchedQueue<'a>(tl, rBack) |> BatchedQueue.checkf)
        | _ -> raise Exceptions.Empty

    ///returns option first element and tail
    member this.TryUncons =  
        match front with
        | hd::tl -> Some(hd, (BatchedQueue<'a>(tl, rBack) |> BatchedQueue.checkf))
        | _ -> None

    with
    interface IQueue<'a> with

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
          
    interface IEnumerable<'a> with

        member this.GetEnumerator() = 
            let e = seq {
                  yield! front
                  yield! (List.rev rBack)}
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BatchedQueue =
    //pattern discriminators
    let (|Cons|Nil|) (q : BatchedQueue<'a>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    ///returns queue of no elements
    ///c is front-back stream ration constant, should be at least 2
    let empty() : BatchedQueue<'a> = BatchedQueue.Empty

    ///applies a function to each element of the queue, threading an accumulator argument through the computation, left to right
    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : BatchedQueue<'T>) = BatchedQueue<_>.fold f state q

    ///applies a function to each element of the queue, threading an accumulator argument through the computation, right to left
    let foldBack (f : ('T -> 'State -> 'State)) (q : BatchedQueue<'T>) (state : 'State) =  BatchedQueue<_>.foldBack f q state

    ///returns the first element
    let inline head (q : BatchedQueue<'a>) = q.Head

    ///returns option first element
    let inline tryGetHead (q : BatchedQueue<'a>) = q.TryGetHead

    ///returns true if the queue has no elements
    let inline isEmpty (q : BatchedQueue<'a>) = q.IsEmpty

    ///returns the count of elememts
    let inline length (q : BatchedQueue<'a>) = q.Length

    ///returns a queue of the list
    let ofList xs = BatchedQueue.OfList xs

    ///returns a queue of the seq
    let ofSeq xs = BatchedQueue.OfSeq xs

    ///returns queue reversed
    let inline rev (q : BatchedQueue<'a>) = q.Rev

    ///returns a new queue with the element added to the end
    let inline snoc (x : 'a) (q : BatchedQueue<'a>) = (q.Snoc x) 

    ///returns a new queue of the elements trailing the first element
    let inline tail (q : BatchedQueue<'a>) = q.Tail 

    ///returns option queue of the elements trailing the first element
    let inline tryGetTail (q : BatchedQueue<'a>) = q.TryGetTail 

    ///returns the first element and tail
    let inline uncons (q : BatchedQueue<'a>) = q.Uncons

    ///returns option first element and tail
    let inline tryUncons (q : BatchedQueue<'a>) = q.TryUncons