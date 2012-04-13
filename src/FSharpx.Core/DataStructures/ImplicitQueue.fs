// implicit queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/18/implicit-queue
module FSharpx.DataStructures.ImplicitQueue

open FSharpx

type Digit<'a> = 
| Zero 
| One of 'a 
| Two of 'a * 'a

type ImplicitQueue<'a> =
| Shallow of Digit<'a>
| Deep of Digit<'a> * Lazy<ImplicitQueue<'a * 'a>> * Digit<'a>

let empty = Shallow Zero
let isEmpty = function Shallow Zero -> true | _ -> false

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

let inline snoc x queue = ImplicitQueue.snoc x queue
let inline head queue = ImplicitQueue<'a>.head queue
let inline tail queue = ImplicitQueue<'a>.tail queue