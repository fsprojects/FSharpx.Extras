// implicit queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/18/implicit-queue
module FSharpx.Collections.Experimental.ImplicitQueue

open FSharpx

type Digit<'T> = 
    | Zero 
    | One of 'T 
    | Two of 'T * 'T

type ImplicitQueue<'T> =
    | Shallow of Digit<'T>
    | Deep of Digit<'T> * Lazy<ImplicitQueue<'T * 'T>> * Digit<'T>

///O(1). Returns queue of no elements.
let empty = Shallow Zero

///O(1). Returns true if the queue has no elements
let isEmpty = function Shallow Zero -> true | _ -> false

type ImplicitQueue<'T> with
    //polymorphic recursion cannot be achieved through let-bound functions
    //hence we use static member methods
    static member snoc (x:'T) : ImplicitQueue<'T> -> ImplicitQueue<'T> = function
        | Shallow Zero -> Shallow (One x)
        | Shallow (One y) -> Deep (Two (y, x), lazy empty, Zero)
        | Deep(f, m, Zero) -> Deep(f, m, One x)
        | Deep(f, m, One y) -> Deep(f, lazy ImplicitQueue.snoc (y, x) (Lazy.force m), Zero)
        | _ -> failwith "should not get there"

    static member head : ImplicitQueue<'T> -> 'T = function
        | Shallow Zero -> raise Exceptions.Empty
        | Shallow (One x) -> x
        | Deep(One x, m, r) -> x
        | Deep(Two(x, y), m, r) -> x
        | _ -> failwith "should not get there"

    static member tryGetHead : ImplicitQueue<'T> -> 'T option = function
        | Shallow Zero -> None
        | Shallow (One x) -> Some x
        | Deep(One x, m, r) -> Some x
        | Deep(Two(x, y), m, r) -> Some x
        | _ -> failwith "should not get there"

    static member tail : ImplicitQueue<'T> -> ImplicitQueue<'T> = function
        | Shallow Zero -> raise Exceptions.Empty
        | Shallow (One x) -> empty
        | Deep(Two(x, y), m, r) -> Deep(One y, m, r)
        | Deep(One x, q, r) ->
            let q' = Lazy.force q
            if isEmpty q' then Shallow r else
            let y, z = ImplicitQueue.head q'
            Deep(Two(y, z), lazy ImplicitQueue.tail q', r)
        | _ -> failwith "should not get there"

    static member tryGetTail : ImplicitQueue<'T> -> ImplicitQueue<'T> option = function
        | Shallow Zero -> None
        | Shallow (One x) -> Some empty
        | Deep(Two(x, y), m, r) -> Some(Deep(One y, m, r))
        | Deep(One x, q, r) ->
            let q' = Lazy.force q
            if isEmpty q' then Some(Shallow r) else
            let y, z = ImplicitQueue.head q'
            Some(Deep(Two(y, z), lazy ImplicitQueue.tail q', r))
        | _ -> failwith "should not get there"

///O(1), amortized. Returns a new queue with the element added to the end.
let inline snoc x queue = ImplicitQueue.snoc x queue

///O(1), amortized. Returns the first element.
let inline head queue = ImplicitQueue<'T>.head queue

///O(1), amortized. Returns option first element.
let inline tryGetHead queue = ImplicitQueue<'T>.tryGetHead queue

///O(1), amortized. Returns a new queue of the elements trailing the first element.
let inline tail queue = ImplicitQueue<'T>.tail queue

///O(1), amortized. Returns option queue of the elements trailing the first element.
let inline tryGetTail queue = ImplicitQueue<'T>.tryGetTail queue