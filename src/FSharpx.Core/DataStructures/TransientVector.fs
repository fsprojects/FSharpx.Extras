/// A transient vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java
module FSharpx.DataStructures.TransientVector

open FSharpx
open FSharpx.DataStructures.Vector

let inline count (vector:TransientVector<'a>) : int  = (vector :> IVector<'a>).Count()

let empty<'a> = TransientVector<'a>()

let inline nth<'a> i (vector:TransientVector<'a>) : 'a = vector.nth i

let inline conj<'a> (x:'a) (vector:TransientVector<'a>) = vector.conj x

let inline peek<'a> (vector:TransientVector<'a>) = (vector :> IVector<'a>).Peek()

let inline pop<'a> (vector:TransientVector<'a>) = vector.pop()

let inline assocN<'a> i (x:'a) (vector:TransientVector<'a>) : TransientVector<'a> = vector.assocN(i,x)