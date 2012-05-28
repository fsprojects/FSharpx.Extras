/// A transient vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java
module FSharpx.DataStructures.TransientVector

open FSharpx
open FSharpx.DataStructures.Vector

let newNode() = Array.create 32 null    

let emptyNode = newNode()


let inline count (vector:TransientVector<'a>) = vector.Count

let empty<'a> = TransientVector<'a>(0,5,emptyNode,Array.create 32 null)

let rec newPath(level,node:Node) =
    if level = 0 then node else
    let ret = newNode()
    ret.[0] <- newPath(level - 5,node) :> obj
    ret

let rec pushTail(vector:TransientVector<'a>,level,parent:Node,tailnode) =
    //if parent is leaf, insert node,
    // else does it map to an existing child? -> nodeToInsert = pushNode one more level
    // else alloc new path
    //return  nodeToInsert placed in copy of parent
    
    let subidx = ((vector.Count - 1) >>> level) &&& 0x01f
    let ret = Array.copy parent

    let nodeToInsert =
        if level = 5 then tailnode else

        let child = parent.[subidx]
        if child <> null then
            pushTail(vector,level-5,child :?> Node,tailnode)
        else
            newPath(level-5,tailnode)

    ret.[subidx] <- nodeToInsert :> obj
    ret

let inline internal arrayFor<'a> i (vector:TransientVector<'a>) =
    if i >= 0 && i < vector.Count then
        if i >= vector.TailOff then vector.Tail else
            let mutable node = vector.Root
            let mutable level = vector.Shift
            while level > 0 do
                let pos = (i >>> level) &&& 0x01f
                node <- node.[pos] :?> Node
                level <- level - 5

            node
    else raise Exceptions.OutOfBounds

let nth<'a> i (vector:TransientVector<'a>) : 'a =
    vector.EnsureEditable()
    let node = arrayFor i vector
    node.[i &&& 0x01f] :?> 'a

let conj<'a> (x:'a) (vector:TransientVector<'a>) =
    vector.EnsureEditable()

    //room in tail?
    if vector.Count - vector.TailOff < 32 then
        vector.Tail.[vector.Count &&& 0x01f] <- x :> obj
        vector.IncCount()
        vector
    else
        //full tail, push into tree
        let tailNode = vector.Tail
        let newShift = vector.Shift
        let tail = Array.create 32 null
        tail.[0] <- x :> obj

        //overflow root?
        let newRoot = 
            if (vector.Count >>> 5) > (1 <<< vector.Shift) then
                let newRoot = newNode()
                newRoot.[0] <- vector.Root :> obj
                newRoot.[1] <- newPath(vector.Shift,tailNode) :> obj
                vector.SetShift(vector.Shift + 5)
                newRoot
            else
                pushTail(vector,vector.Shift,vector.Root,tailNode)

        vector.SetTail tail
        vector.SetRoot newRoot
        vector.IncCount()
        vector


let rec doAssoc(level,node:Node,i,x) =
    let ret = node
    if level = 0 then 
        ret.[i &&& 0x01f] <- x :> obj 
    else
        let subidx = (i >>> level) &&& 0x01f
        ret.[subidx] <- doAssoc(level - 5, node.[subidx] :?> Node, i, x) :> obj
    ret

let assocN<'a> i (x:'a) (vector:TransientVector<'a>) : TransientVector<'a> =
    vector.EnsureEditable()
    if i >= 0 && i < vector.Count then
        if i >= vector.TailOff then
            vector.Tail.[i &&& 0x01f] <- x :> obj
            vector
        else
            vector.SetRoot(doAssoc(vector.Shift, vector.Root, i, x))
            vector
    elif i = vector.Count then
        conj x vector
    else raise Exceptions.OutOfBounds

let rangedIterator<'a> startIndex endIndex (vector:TransientVector<'a>) : 'a seq =
    let i = ref startIndex
    let b = ref (!i - (!i % 32))
    let array = if startIndex < vector.Count then ref (arrayFor !i vector) else ref null

    seq {
        while !i < endIndex do
            if !i - !b = 32 then
                array := arrayFor !i vector
                b := !b + 32

            yield (!array).[!i &&& 0x01f] :?> 'a
            i := !i + 1 
       }

let toSeq (vector:TransientVector<'a>) = rangedIterator 0 vector.Count vector

let persistent (vector:TransientVector<'a>) =
    vector.EnsureEditable()
    // TODO: root.edit.set(null)
    let l = vector.Count - vector.TailOff
    let trimmedTail = Array.init l (fun i -> vector.Tail.[i])
    PersistentVector(vector.Count, vector.Shift, vector.Root, trimmedTail)