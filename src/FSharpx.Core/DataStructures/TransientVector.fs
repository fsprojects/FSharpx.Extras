/// A transient vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java
module FSharpx.DataStructures.TransientVector

open FSharpx
open FSharpx.DataStructures.Vector

let newNode() = Array.create 32 null    

let emptyNode = newNode()


let inline count (vector:TransientVector<'a>) = vector.Count

let empty<'a> = TransientVector<'a>(0,5,emptyNode,Array.create 32 null)

let inline nth<'a> i (vector:TransientVector<'a>) : 'a = vector.nth i

let inline conj<'a> (x:'a) (vector:TransientVector<'a>) = vector.conj x

let inline assocN<'a> i (x:'a) (vector:TransientVector<'a>) : TransientVector<'a> = vector.assocN i x