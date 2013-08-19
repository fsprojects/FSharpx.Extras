//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/01/21/hood-melville-queue

//jf -- added ofSeq, ofList, Uncons, and try...
//pattern discriminators Snoc and Nil

namespace FSharpx.DataStructures

#nowarn "44"
open System.Collections
open System.Collections.Generic

type RotationState<'a> =
    | Idle
    | Reversing of int * list<'a> * list<'a> * list<'a> * list<'a>
    | Appending of int *list<'a> * list<'a>
    | Done of list<'a>

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
type HoodMelvilleQueue<'a> (frontLength : int, front : list<'a>, state : RotationState<'a>, rBackLength : int, rBack : list<'a>) = 

    member private this.frontLength = frontLength

    member internal this.front = front

    member private this.state = state

    member private this.rBackLength = rBackLength

    member internal this.rBack = rBack

    static member private exec : RotationState<'a> -> RotationState<'a> = function
        | Reversing(ok, x::f, f', y::r, r') -> Reversing(ok+1, f, x::f', r, y::r')
        | Reversing(ok, [], f', [y], r') -> Appending(ok, f', y::r')
        | Appending(0, f', r') -> Done r'
        | Appending(ok, x::f', r') -> Appending(ok-1, f', x::r')
        | state -> state

    static member private invalidate : RotationState<'a> -> RotationState<'a> = function
        | Reversing(ok, f, f', r, r') -> Reversing(ok-1, f, f', r, r')
        | Appending(0, f', x::r') -> Done r'
        | Appending(ok, f', r') -> Appending(ok-1, f', r')
        | state -> state

    static member private exec2 (q : HoodMelvilleQueue<'a>) =
        match (HoodMelvilleQueue.exec (HoodMelvilleQueue.exec q.state)) with
        | Done newf -> HoodMelvilleQueue(q.frontLength , newf, Idle, q.rBackLength, q.rBack) 
        | newstate -> HoodMelvilleQueue(q.frontLength , q.front, newstate, q.rBackLength, q.rBack) 

    static member private check (q : HoodMelvilleQueue<'a>) =
        if q.rBackLength <= q.frontLength then
          HoodMelvilleQueue.exec2 q
        else
          let newstate = Reversing(0, q.front, [], q.rBack, [])
          HoodMelvilleQueue((q.frontLength + q.rBackLength), q.front, newstate, 0, []) |> HoodMelvilleQueue.exec2

    static member internal readyQ (q : HoodMelvilleQueue<'a>) = 
        let rec loop (q' : HoodMelvilleQueue<'a>) =
            match q'.state with
            | Done _ | Idle -> q'
            | _ -> loop (HoodMelvilleQueue.check q')
        loop q

    static member internal Empty() : HoodMelvilleQueue<'a> = HoodMelvilleQueue(0, [], Idle, 0, [])

    static member internal fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : HoodMelvilleQueue<'T>)  :  'State =     
        let q' = HoodMelvilleQueue.readyQ q
        let s = List.fold f state q'.front
        List.fold f s (List.rev q'.rBack)

    static member internal foldBack (f : ('T -> 'State -> 'State)) (q : HoodMelvilleQueue<'T>) (state : 'State) :  'State = 
        let q' = HoodMelvilleQueue.readyQ q
        let s = List.foldBack f (List.rev q'.rBack) state 
        List.foldBack f q'.front s

    static member internal OfList (xs:list<'a>) = HoodMelvilleQueue<'a>(xs.Length, xs, Idle, 0, [])

    static member internal OfSeq (xs:seq<'a>) = 
        HoodMelvilleQueue<'a>((Seq.length xs), (List.ofSeq xs), Idle, 0, [])
   
    ///returns the first element
    member this.Head  =
        match front with
        | hd::_ -> hd
        | _ -> raise Exceptions.Empty

    ///returns option first element
    member this.TryGetHead =
        match front with
        | hd::_ -> Some(hd)
        | _ -> None
         
    ///returns true if the queue has no elements
    member this.IsEmpty = (frontLength = 0)

    ///returns the count of elememts
    member this.Length = frontLength + rBackLength

    ///returns a new queue with the element added to the end
    member this.Snoc x = 
        HoodMelvilleQueue<'a>(frontLength, front, state, (rBackLength + 1), (x::rBack))
        |> HoodMelvilleQueue.check

    ///returns a new queue of the elements trailing the first element
    member this.Tail =
        match front with
        | hd::tl ->
            HoodMelvilleQueue<'a>((frontLength-1), tl, (HoodMelvilleQueue.invalidate state), rBackLength, rBack)
            |> HoodMelvilleQueue.check
        | _ -> raise Exceptions.Empty

    ///returns option queue of the elements trailing the first element
    member this.TryGetTail =
        match front with
        | hd::tl ->
            Some(HoodMelvilleQueue<'a>((frontLength-1), tl, (HoodMelvilleQueue.invalidate state), rBackLength, rBack)
            |> HoodMelvilleQueue.check)
        | _ -> None

    ///returns the first element and tail
    member this.Uncons =  
        match front with
        | hd::tl ->
            hd, (HoodMelvilleQueue<'a>((frontLength-1), tl, (HoodMelvilleQueue.invalidate state), rBackLength, rBack) |> HoodMelvilleQueue.check)
        | _ -> raise Exceptions.Empty

    ///returns option first element and tail
    member this.TryUncons =  
        match front with
        | hd::tl ->
            Some(hd, (HoodMelvilleQueue<'a>((frontLength-1), tl, (HoodMelvilleQueue.invalidate state), rBackLength, rBack)
            |> HoodMelvilleQueue.check))
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
            let e = 
                let q = HoodMelvilleQueue.readyQ this
                
                seq {
                    yield! q.front
                    yield! (List.rev q.rBack)  }
                   
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HoodMelvilleQueue =
    //pattern discriminators
    let (|Cons|Nil|) (q : HoodMelvilleQueue<'a>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    ///returns queue of no elements
    let empty() = HoodMelvilleQueue.Empty()

    ///applies a function to each element of the queue, threading an accumulator argument through the computation, left to right
    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : HoodMelvilleQueue<'T>) = HoodMelvilleQueue<_>.fold f state q

    ///applies a function to each element of the queue, threading an accumulator argument through the computation, right to left
    let foldBack (f : ('T -> 'State -> 'State)) (q : HoodMelvilleQueue<'T>) (state : 'State) =  HoodMelvilleQueue<_>.foldBack f q state

    ///returns the first element
    let inline head (q : HoodMelvilleQueue<'a>) = q.Head

    ///returns option first element
    let inline tryGetHead (q : HoodMelvilleQueue<'a>) = q.TryGetHead

    ///returns true if the queue has no elements

    let inline isEmpty (q : HoodMelvilleQueue<'a>) = q.IsEmpty

    ///returns the count of elememts
    let inline length (q : HoodMelvilleQueue<'a>) = q.Length

    ///returns a queue of the list
    let ofList xs = HoodMelvilleQueue.OfSeq xs

    ///returns a queue of the seq
    let ofSeq xs = HoodMelvilleQueue.OfSeq xs

    ///returns a new queue with the element added to the end
    let inline snoc (x : 'a) (q : HoodMelvilleQueue<'a>) = (q.Snoc x) 

    ///returns a new queue of the elements trailing the first element
    let inline tail (q : HoodMelvilleQueue<'a>) = q.Tail 

    ///returns option queue of the elements trailing the first element
    let inline tryGetTail (q : HoodMelvilleQueue<'a>) = q.TryGetTail 

    ///returns the first element and tail
    let inline uncons (q : HoodMelvilleQueue<'a>) = q.Uncons

    ///returns option first element and tail
    let inline tryUncons (q : HoodMelvilleQueue<'a>) = q.TryUncons