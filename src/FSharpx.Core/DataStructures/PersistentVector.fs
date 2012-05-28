/// A persistent vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java
module FSharpx.DataStructures.PersistentVector

open FSharpx
open FSharpx.DataStructures.Vector

let newNode() = Array.create 32 null    

let emptyNode = newNode()

let inline count (vector:PersistentVector<'a>) = vector.Count

let empty<'a> = PersistentVector<'a>(0,5,emptyNode,[||])

let inline nth<'a> i (vector:PersistentVector<'a>) : 'a = vector.nth i
    
let inline cons<'a> (x:'a) (vector:PersistentVector<'a>) = vector.cons x

let inline assocN<'a> i (x:'a) (vector:PersistentVector<'a>) : PersistentVector<'a> = vector.assocN i x

open FSharpx.DataStructures.TransientVector

let inline ofSeq (items:'a seq) = PersistentVector.ofSeq items