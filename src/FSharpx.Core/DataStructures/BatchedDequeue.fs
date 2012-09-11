//inspired by, and init and tail algorithms from,  http://fssnip.net/dJ

//pattern discriminators Cons, Snoc, and Nil

module FSharpx.DataStructures.BatchedDequeue

open System.Collections
open System.Collections.Generic

type BatchedDequeue<'a> (front, rBack) = 

    static member private dummy = "x" //compiler wants at least one member
        
    with
    interface IDequeue<'a> with

        member this.Cons x =  BatchedDequeue(x::front, rBack) :> _
            
        member this.Head() =
            match front, rBack with
            | [], [] -> raise Exceptions.Empty
            | hd::tl, _ -> hd
            | [], xs -> List.rev xs |> List.head

        member this.Init() = 
            match front, rBack with 
            | [],  [] -> raise Exceptions.Empty
            | _ , x::xs -> BatchedDequeue(front, xs) :> _
            | _ ,   [] ->       //splits front in two, favoring frontbot for odd length
                let _, fronttop, frontbot = List.fold(fun (i, reartop, rearbot) e -> 
                    if i < rBack.Length /2
                    then (i+1, e::reartop, rearbot)
                    else (i+1, reartop, e::rearbot)) (0,[],[]) front
                let front', rear' = fronttop |> List.rev , frontbot
                BatchedDequeue(front', rear'.Tail) :> _         
            
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
            match front, rBack with
            | [],  [] -> raise Exceptions.Empty
            | x::xs,  _ ->  BatchedDequeue(xs, rBack) :> _
            | _,  _ ->      //splits rear in two, favoring rearbot for odd length
                let _, reartop, rearbot = 
                    List.fold(fun (i, reartop, rearbot) e -> 
                        if i < rBack.Length / 2 
                        then (i+1, e::reartop, rearbot)
                        else (i+1, reartop, e::rearbot)) (0,[],[]) rBack
                let rear', front' = reartop |> List.rev, rearbot 
                BatchedDequeue(front'.Tail, rear') :> _

        member this.Snoc x = BatchedDequeue(front, x::rBack) :> _

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
                  yield! (List.rev rBack)}
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

    ///returns a new deque with the element added to the beginning
    member this.Cons x = ((this :> 'a IDequeue).Cons x) :?> BatchedDequeue<'a>

    ///returns the first element
    member this.Head() = (this :> 'a IDequeue).Head()

    ///returns a new deque of the elements before the last element
    member this.Init() = ((this :> 'a IDequeue).Init()) :?> BatchedDequeue<'a>

    ///returns true if the deque has no elements
    member this.IsEmpty() = (this :> 'a IDequeue).IsEmpty()

    ///returns the last element
    member this.Last() = (this :> 'a IDequeue).Last()

    ///returns the count of elememts
    member this.Length() = (this :> 'a IDequeue).Length()

    ///returns a new deque with the element added to the end
    member this.Snoc x = ((this :> 'a IDequeue).Snoc x) :?> BatchedDequeue<'a>

    ///returns a new deque of the elements trailing the first element
    member this.Tail() = ((this :> 'a IDequeue).Tail()) :?> BatchedDequeue<'a>

    ///returns the first element and tail
    member this.Uncons() = 
        let x, xs = (this :> 'a IDequeue).Uncons() 
        x, xs :?> BatchedDequeue<'a>

    ///returns init and the last element
    member this.Unsnoc() = 
        let xs, x = (this :> 'a IDequeue).Unsnoc() 
        xs :?> BatchedDequeue<'a>, x

//pattern discriminator

let private getDequeCons (q : BatchedDequeue<'a>) = 
    if q.IsEmpty() then None
    else Some(q.Head(), q.Tail())

let private getDequeSnoc (q : BatchedDequeue<'a>) = 
    if q.IsEmpty() then None
    else Some(q.Init(), q.Last())

let (|Cons|Nil|) q = match getDequeCons q with Some(a,b) -> Cons(a,b) | None -> Nil

let (|Snoc|Nil|) q = match getDequeSnoc q with Some(a,b) -> Snoc(a,b) | None -> Nil

///returns a new deque with the element added to the beginning
let inline cons (x : 'a) (q : BatchedDequeue<'a>) = q.Cons x 

//returns deque of no elements
let inline empty() = BatchedDequeue([], [])

///returns the first element
let inline head (q : BatchedDequeue<'a>) = q.Head()

///returns a new deque of the elements before the last element
let inline init (q : BatchedDequeue<'a>) = q.Init() 

///returns true if the deque has no elements
let inline isEmpty (q : BatchedDequeue<'a>) = q.IsEmpty()

///returns the last element
let inline last (q : BatchedDequeue<'a>) = q.Last()

///returns the count of elememts
let inline length (q : BatchedDequeue<'a>) = q.Length()

///returns a deque of the two lists concatenated
let inline ofCatLists xs ys = BatchedDequeue(xs, List.rev ys)

//returns a deque of one element
let inline singleton x = BatchedDequeue([x], [])

///returns a new deque with the element added to the end
let inline snoc (x : 'a) (q : BatchedDequeue<'a>) = (q.Snoc x) 

///returns a new deque of the elements trailing the first element
let inline tail (q : BatchedDequeue<'a>) = q.Tail() 

///returns the first element and tail
let inline uncons (q : BatchedDequeue<'a>) = q.Uncons()

///returns init and the last element
let inline unsnoc (q : BatchedDequeue<'a>) = q.Unsnoc()