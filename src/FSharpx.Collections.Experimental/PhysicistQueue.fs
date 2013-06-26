//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/31/physicist-queue

//jf -- added ofSeq and try...
//pattern discriminators Snoc and Nil

namespace FSharpx.Collections.Experimental

open System.Collections
open System.Collections.Generic
 
type PhysicistQueue<'T> (prefix : list<'T>, frontLength : int, front : Lazy<list<'T>>, rBackLength : int, rBack : list<'T>) = 

    member internal this.prefix = prefix

    member private this.frontLength = frontLength

    member internal this.front = front

    member private this.rBackLength = rBackLength

    member internal this.rBack = rBack

    static member private checkw (q : PhysicistQueue<'T>) =
        match q.prefix with
        | [] -> PhysicistQueue(q.front.Value, q.frontLength, q.front, q.rBackLength, q.rBack)
        | _ -> q

    static member private check (q : PhysicistQueue<'T>) =
        if q.rBackLength <= q.frontLength then
            PhysicistQueue.checkw q
        else
            PhysicistQueue(q.front.Value, (q.frontLength + q.rBackLength),  (lazy (q.rBack |> List.rev |> List.append q.front.Value)), 0, [])
            |> PhysicistQueue.checkw

    static member private length (q : PhysicistQueue<'T>) = q.frontLength + q.rBackLength

    static member internal Empty() = PhysicistQueue<'T>([], 0, lazy [], 0, []) 

    static member internal fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : PhysicistQueue<'T>)  :  'State =        
        let s = 
             if (q.prefix.Length = q.frontLength)
                then List.fold f state q.prefix
                else List.fold f state q.front.Value

        List.fold f s (List.rev q.rBack)

    static member internal foldBack (f : ('T -> 'State -> 'State)) (q : PhysicistQueue<'T>) (state : 'State) :  'State = 
        let s = List.foldBack f (List.rev q.rBack) state 

        if (q.prefix.Length = q.frontLength)
            then (List.foldBack f q.prefix s)
            else (List.foldBack f q.front.Value s)

    static member internal OfList (xs:list<'T>) = PhysicistQueue<'T>(xs, xs.Length, (lazy xs), 0, [])

    static member internal OfSeq (xs:seq<'T>) = 
        PhysicistQueue<'T>((List.ofSeq xs), (Seq.length xs), (lazy (List.ofSeq xs)), 0, [])
   
    ///O(1), amortized. Returns the first element.
    member this.Head =
        match prefix with
        | hd::_ -> hd
        | _ -> raise Exceptions.Empty

    ///O(1), amortized. Returns option first element.
    member this.TryGetHead =
        match prefix with
        | hd::_ -> Some(hd)
        | _ -> None
         
    ///O(1). Returns true if the queue has no elements.
    member this.IsEmpty = (frontLength = 0)

    ///O(1). Returns the count of elememts.
    member this.Length = PhysicistQueue.length this

    ///O(1). Returns queue reversed.
    member this.Rev = 
        if (prefix.Length = frontLength)
        then PhysicistQueue<'T>(rBack, rBackLength, (lazy rBack), frontLength, prefix) |> PhysicistQueue.check
        else PhysicistQueue<'T>(rBack, rBackLength, (lazy rBack), frontLength, front.Value) |> PhysicistQueue.check

    ///O(1), amortized. Returns a new queue with the element added to the end.
    member this.Snoc x = 
        PhysicistQueue(prefix, frontLength, front, (rBackLength + 1), x::rBack)
        |> PhysicistQueue.check

    ///O(1), amortized. Returns a new queue of the elements trailing the first element.
    member this.Tail =
        match prefix with
        | hd::_ ->
            PhysicistQueue(prefix.Tail, (frontLength - 1), (lazy front.Value.Tail), rBackLength, rBack)
            |> PhysicistQueue.check
        | _ -> raise Exceptions.Empty

    ///O(1), amortized. Returns option queue of the elements trailing the first element.
    member this.TryGetTail =
        match prefix with
        | hd::_ ->
            Some (PhysicistQueue(prefix.Tail, (frontLength - 1), (lazy front.Value.Tail), rBackLength, rBack) |> PhysicistQueue.check)
        | _ -> None

    ///O(1), amortized. Returns the first element and tail.
    member this.Uncons = 
        match prefix with
        | hd::_ -> hd, (PhysicistQueue(prefix.Tail, (frontLength - 1), (lazy front.Value.Tail), rBackLength, rBack) |> PhysicistQueue.check)
        | _ -> raise Exceptions.Empty

    ///O(1), amortized. Returns option first element and tail.
    member this.TryUncons =  
       match prefix with
        | hd::_ -> Some(hd, (PhysicistQueue(prefix.Tail, (frontLength - 1), (lazy front.Value.Tail), rBackLength, rBack) |> PhysicistQueue.check))
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
                  yield! front.Value
                  yield! (List.rev rBack)}
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PhysicistQueue =
    //pattern discriminators

    let (|Cons|Nil|) (q : PhysicistQueue<'T>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    ///O(1). Returns queue of no elements.
    let empty() = PhysicistQueue.Empty()

    ///O(n). Applies a function to each element of the queue, threading an accumulator argument through the computation, left to right.
    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : PhysicistQueue<'T>) = PhysicistQueue<_>.fold f state q

    ///O(n). Applies a function to each element of the queue, threading an accumulator argument through the computation, right to left.
    let foldBack (f : ('T -> 'State -> 'State)) (q : PhysicistQueue<'T>) (state : 'State) =  PhysicistQueue<_>.foldBack f q state

    ///O(1), amortized. Returns the first element.
    let inline head (q : PhysicistQueue<'T>) = q.Head

    ///O(1), amortized. Returns option first element.
    let inline tryGetHead (q : PhysicistQueue<'T>) = q.TryGetHead

    ///O(1). Returns true if the queue has no elements.
    let inline isEmpty (q : PhysicistQueue<'T>) = q.IsEmpty

    ///O(1). Returns the count of elememts.
    let inline length (q : PhysicistQueue<'T>) = q.Length

    ///O(1). Returns a queue of the list.
    let ofList xs = PhysicistQueue.OfList xs

    ///O(1). Returns a queue of the seq.
    let ofSeq xs = PhysicistQueue.OfSeq xs

    ///O(1). Returns queue reversed.
    let inline rev (q : PhysicistQueue<'T>) = q.Rev

    ///O(1), amortized. Returns a new queue with the element added to the end.
    let inline snoc (x : 'T) (q : PhysicistQueue<'T>) = (q.Snoc x) 

    ///O(1), amortized. Returns a new queue of the elements trailing the first element.
    let inline tail (q : PhysicistQueue<'T>) = q.Tail 

    ///O(1), amortized. Returns option queue of the elements trailing the first element.
    let inline tryGetTail (q : PhysicistQueue<'T>) = q.TryGetTail 

    ///O(1), amortized. Returns the first element and tail.
    let inline uncons (q : PhysicistQueue<'T>) = q.Uncons

    ///O(1), amortized. Returns option first element and tail.
    let inline tryUncons (q : PhysicistQueue<'T>) = q.TryUncons