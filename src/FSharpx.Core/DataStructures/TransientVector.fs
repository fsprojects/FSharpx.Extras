/// A transient vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java
module FSharpx.DataStructures.TransientVector

open FSharpx
open FSharpx.DataStructures.Vector

/// Returns the number of items in the collection.
let inline count (vector:TransientVector<'a>) : int  = (vector :> IVector<'a>).Count()

let empty<'a> = TransientVector<'a>()

/// Returns the value at the index. If the index is out of bounds it throws an exception.
let inline nth<'a> i (vector:TransientVector<'a>) : 'a = vector.nth i

/// Returns a new vector with the element 'added' at the end.
let inline conj<'a> (x:'a) (vector:TransientVector<'a>) = vector.conj x

/// Returns the last element in the vector. If the vector is empty it throws an exception.
let inline peek<'a> (vector:TransientVector<'a>) = (vector :> IVector<'a>).Peek()

/// Returns a new vector without the last item. If the collection is empty it throws an exception.
let inline pop<'a> (vector:TransientVector<'a>) = vector.pop()

/// Returns a new vector that contains the given value at the index. Note - index must be <= vector.Count.
let inline assocN<'a> i (x:'a) (vector:TransientVector<'a>) : TransientVector<'a> = vector.assocN(i,x)