// implicit queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/18/implicit-queue
[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
module FSharpx.DataStructures.ImplicitQueue

#nowarn "44"
open FSharpx

type Digit<'a> = 
| Zero 
| One of 'a 
| Two of 'a * 'a

type ImplicitQueue<'a> =
| Shallow of Digit<'a>
| Deep of Digit<'a> * Lazy<ImplicitQueue<'a * 'a>> * Digit<'a>

///O(1). Returns queue of no elements.
let empty = Shallow Zero

///O(1). Returns true if the queue has no elements
let isEmpty = function Shallow Zero -> true | _ -> false

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
type ImplicitQueue<'a> with
    //polymorphic recursion cannot be achieved through let-bound functions
    //hence we use static member methods
    static member snoc (x:'a) : ImplicitQueue<'a> -> ImplicitQueue<'a> = function
        | Shallow Zero -> Shallow (One x)
        | Shallow (One y) -> Deep (Two (y, x), lazy empty, Zero)
        | Deep(f, m, Zero) -> Deep(f, m, One x)
        | Deep(f, m, One y) -> Deep(f, lazy ImplicitQueue.snoc (y, x) (Lazy.force m), Zero)
        | _ -> failwith "should not get there"

    static member head : ImplicitQueue<'a> -> 'a = function
        | Shallow Zero -> raise Exceptions.Empty
        | Shallow (One x) -> x
        | Deep(One x, m, r) -> x
        | Deep(Two(x, y), m, r) -> x
        | _ -> failwith "should not get there"

    static member tryGetHead : ImplicitQueue<'a> -> 'a option = function
        | Shallow Zero -> None
        | Shallow (One x) -> Some x
        | Deep(One x, m, r) -> Some x
        | Deep(Two(x, y), m, r) -> Some x
        | _ -> failwith "should not get there"

    static member tail : ImplicitQueue<'a> -> ImplicitQueue<'a> = function
        | Shallow Zero -> raise Exceptions.Empty
        | Shallow (One x) -> empty
        | Deep(Two(x, y), m, r) -> Deep(One y, m, r)
        | Deep(One x, q, r) ->
            let q' = Lazy.force q
            if isEmpty q' then Shallow r else
            let y, z = ImplicitQueue.head q'
            Deep(Two(y, z), lazy ImplicitQueue.tail q', r)
        | _ -> failwith "should not get there"

    static member tryGetTail : ImplicitQueue<'a> -> ImplicitQueue<'a> option = function
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
let inline head queue = ImplicitQueue<'a>.head queue

///O(1), amortized. Returns option first element.
let inline tryGetHead queue = ImplicitQueue<'a>.tryGetHead queue

///O(1), amortized. Returns a new queue of the elements trailing the first element.
let inline tail queue = ImplicitQueue<'a>.tail queue

///O(1), amortized. Returns option queue of the elements trailing the first element.
let inline tryGetTail queue = ImplicitQueue<'a>.tryGetTail queue