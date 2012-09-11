//originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/17/double-ended-queue

//jf -- added rev, ofSeq, ofSeqC, lookup, append, appendC, update, remove, tryUpdate, tryRemove

//pattern discriminators Cons, Snoc, and Nil

module FSharpx.DataStructures.Dequeue

open System.Collections
open System.Collections.Generic

type Dequeue<'a> (front, rBack) = 

    static member private splitAux n (r:'a list) (acc:'a list) =
        match r with
        | hd::tl when List.length acc < n ->
            Dequeue.splitAux n tl (hd::acc)
        | _ ->
            List.rev r, List.rev acc

    static member private split (r:'a list) =
        Dequeue.splitAux (List.length r / 2) r []

    static member private checkf : 'a list * 'a list  -> 'a list * 'a list = function
        | [], r -> Dequeue.split r
        | deq -> deq

    static member private checkr : 'a list * 'a list  -> 'a list * 'a list = function
        | f, [] ->
            let a, b = Dequeue.split f
            b, a
        | deq -> deq

    with
    interface IDequeue<'a> with

        member this.Cons x =
            let f, r = Dequeue.checkr (x::front, rBack)
            Dequeue (f, r) :> _ 
   
        member this.Head() =
            match front, rBack with
            | [], [] -> raise Exceptions.Empty
            | hd::tl, _ -> hd
            | [], xs -> List.rev xs |> List.head

        member this.Init() = 
            let rec loop : 'a list * 'a list -> Dequeue<'a> = function
                | [], [] -> raise Exceptions.Empty
                | f, hd::tl -> 
                    let f, r = Dequeue.checkr (f, tl)
                    Dequeue (f, r) 
                | hd::[], [] ->   Dequeue ([], []) 
                | f, [] ->  Dequeue.split f |> loop 
            loop (front, rBack) :> _
         
        member this.IsEmpty() =  
            match front, rBack with
            | [], [] -> true | _ -> false

        member this.Last() = 
            match front, rBack with
            | [], [] -> raise Exceptions.Empty
            | xs, [] -> List.rev xs |> List.head
            | _, hd::tl -> hd

        member this.Length() = front.Length + rBack.Length

        member this.Tail() =
            let rec loop : 'a list * 'a list -> Dequeue<'a> = function
                | [], [] -> raise Exceptions.Empty
                | hd::tl, r -> 
                    let f, r = Dequeue.checkf (tl, r)
                    Dequeue (f, r)
                | [], r -> Dequeue.split r |> loop
            loop (front, rBack) :> _

        member this.Snoc x = 
            let f, r = Dequeue.checkf (front, x::rBack)
            Dequeue (f, r) :> _

        member this.Uncons() =  
            match front, rBack with
            | [], [] -> raise Exceptions.Empty
            | _, _ -> this.Head(), this.Tail() :> _

        member this.Unsnoc() =  
            match front, rBack with
            | [], [] -> raise Exceptions.Empty
            | _, _ -> this.Init() :> _, this.Last()
          
    interface IEnumerable<'a> with

        member this.GetEnumerator() = 
            let e = seq {
                  yield! front
                  yield! (List.rev rBack)  }
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

       
    ///returns a new deque with the element added to the beginning
    member this.Cons x = ((this :> 'a IDequeue).Cons x) :?> Dequeue<'a>

    ///returns the first element
    member this.Head() = (this :> 'a IDequeue).Head()

    ///returns a new deque of the elements before the last element
    member this.Init() = ((this :> 'a IDequeue).Init()) :?> Dequeue<'a>

    ///returns true if the deque has no elements
    member this.IsEmpty() = (this :> 'a IDequeue).IsEmpty()

    ///returns the last element
    member this.Last() = (this :> 'a IDequeue).Last()

    ///returns the count of elememts
    member this.Length() = (this :> 'a IDequeue).Length()

    ///returns a new deque with the element added to the end
    member this.Snoc x = ((this :> 'a IDequeue).Snoc x) :?> Dequeue<'a>

    ///returns a new deque of the elements trailing the first element
    member this.Tail() = ((this :> 'a IDequeue).Tail()) :?> Dequeue<'a>

    ///returns the first element and tail
    member this.Uncons() = 
        let x, xs = (this :> 'a IDequeue).Uncons() 
        x, xs :?> Dequeue<'a>

    ///returns init and the last element
    member this.Unsnoc() = 
        let xs, x = (this :> 'a IDequeue).Unsnoc() 
        xs :?> Dequeue<'a>, x

//pattern discriminator

let private getDequeCons (q : Dequeue<'a>) = 
    if q.IsEmpty() then None
    else Some(q.Head(), q.Tail())

let private getDequeSnoc (q : Dequeue<'a>) = 
    if q.IsEmpty() then None
    else Some(q.Init(), q.Last())

let (|Cons|Nil|) q = match getDequeCons q with Some(a,b) -> Cons(a,b) | None -> Nil

let (|Snoc|Nil|) q = match getDequeSnoc q with Some(a,b) -> Snoc(a,b) | None -> Nil

///returns a new deque with the element added to the beginning
let inline cons (x : 'a) (q : Dequeue<'a>) = q.Cons x 

//returns deque of no elements
let inline empty() = Dequeue([], [])

///returns the first element
let inline head (q : Dequeue<'a>) = q.Head()

///returns a new deque of the elements before the last element
let inline init (q : Dequeue<'a>) = q.Init() 

///returns true if the deque has no elements
let inline isEmpty (q : Dequeue<'a>) = q.IsEmpty()

///returns the last element
let inline last (q : Dequeue<'a>) = q.Last()

///returns the count of elememts
let inline length (q : Dequeue<'a>) = q.Length()

///returns a deque of the two lists concatenated
let inline ofCatLists xs ys = Dequeue(xs, List.rev ys)

//returns a deque of one element
let inline singleton x = Dequeue([x], [])

///returns a new deque with the element added to the end
let inline snoc (x : 'a) (q : Dequeue<'a>) = (q.Snoc x) 

///returns a new deque of the elements trailing the first element
let inline tail (q : Dequeue<'a>) = q.Tail() 

///returns the first element and tail
let inline uncons (q : Dequeue<'a>) = q.Uncons()

///returns init and the last element
let inline unsnoc (q : Dequeue<'a>) = q.Unsnoc()