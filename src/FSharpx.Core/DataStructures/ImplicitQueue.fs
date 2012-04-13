// implicit queue from Chris Okasaki’s “Purely functional data structures”
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/18/implicit-queue
namespace FSharpx.DataStructures

open FSharpx

module Exceptions = 
    let Empty = new System.Exception("Queue is empty") // TODO: make this a better exception

module ImplicitQueue =
    type Digit<'a> = Zero | One of 'a | Two of 'a * 'a

    type t<'a> =
    | Shallow of Digit<'a>
    | Deep of Digit<'a> * Lazy<t<'a * 'a>> * Digit<'a>

    let empty = Shallow Zero
    let isEmpty = function Shallow Zero -> true | _ -> false

    type t<'a> with
        //polymorphic recursion cannot be achieved through let-bound functions
        //hence we use static member methods
        static member snoc (x:'a) : t<'a> -> t<'a> = function
            | Shallow Zero -> Shallow (One x)
            | Shallow (One y) -> Deep (Two (y, x), lazy empty, Zero)
            | Deep(f, m, Zero) -> Deep(f, m, One x)
            | Deep(f, m, One y) -> Deep(f, lazy t.snoc (y, x) (Lazy.force m), Zero)
            | _ -> failwith "should not get there"

        static member head : t<'a> -> 'a = function
            | Shallow Zero -> raise Exceptions.Empty
            | Shallow (One x) -> x
            | Deep(One x, m, r) -> x
            | Deep(Two(x, y), m, r) -> x
            | _ -> failwith "should not get there"

        static member tail : t<'a> -> t<'a> = function
            | Shallow Zero -> raise Exceptions.Empty
            | Shallow (One x) -> empty
            | Deep(Two(x, y), m, r) -> Deep(One y, m, r)
            | Deep(One x, q, r) ->
                let q' = Lazy.force q
                if isEmpty q' then Shallow r else
                let y, z = t.head q'
                Deep(Two(y, z), lazy t.tail q', r)
            | _ -> failwith "should not get there"

    let snoc x q = t.snoc x q
    let head q = t.head q
    let tail q = t.tail q
