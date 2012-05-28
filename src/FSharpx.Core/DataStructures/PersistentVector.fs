/// A persistent vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java
module FSharpx.DataStructures.PersistentVector

open FSharpx

type Node = obj[]

let newNode() = Array.create 32 null    

let emptyNode = newNode()

type PersistentVector<'a> =  {
    Count : int
    Shift : int
    Root : Node
    Tail : obj[]
    TailOff: int }

let inline count vector = vector.Count

let inline persistentVector<'a>(count,shift,root,tail) : PersistentVector<'a> = {
    Count = count
    Shift = shift
    Root = root
    Tail = tail 
    TailOff = 
        if count < 32 then 0 else
        ((count - 1) >>> 5) <<< 5 }

let empty<'a> = persistentVector<'a>(0,5,emptyNode,[||])

let rec newPath(level,node:Node) =
    if level = 0 then node else
    let ret = newNode()
    ret.[0] <- newPath(level - 5,node) :> obj
    ret

let rec pushTail(vector,level,parent:Node,tailnode) =
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

let inline arrayFor<'a> i (vector:PersistentVector<'a>) =
    if i >= 0 && i < vector.Count then
        if i >= vector.TailOff then vector.Tail else
            let node = ref vector.Root
            let level = ref vector.Shift
            while !level > 0 do
                let pos = (i >>> !level) &&& 0x01f
                node := (!node).[pos] :?> Node
                level := !level - 5

            !node
    else raise Exceptions.OutOfBounds

let nth<'a> i (vector:PersistentVector<'a>) : 'a =
    let node = arrayFor i vector
    node.[i &&& 0x01f] :?> 'a

let cons<'a> (x:'a) (vector:PersistentVector<'a>) =
    if vector.Count - vector.TailOff < 32 then
        let newTail = Array.append vector.Tail [|x:>obj|]
        persistentVector<'a>(vector.Count + 1,vector.Shift,vector.Root,newTail)
    else
        //full tail, push into tree
        let tailNode = vector.Tail
        let newShift = vector.Shift

        //overflow root?
        if (vector.Count >>> 5) > (1 <<< vector.Shift) then
            let newRoot = newNode()
            newRoot.[0] <- vector.Root :> obj
            newRoot.[1] <- newPath(vector.Shift,tailNode) :> obj
            persistentVector<'a>(vector.Count + 1,vector.Shift + 5,newRoot,[| x |])
        else
            let newRoot = pushTail(vector,vector.Shift,vector.Root,tailNode)
            persistentVector<'a>(vector.Count + 1,vector.Shift,newRoot,[| x |])


let rec doAssoc(level,node:Node,i,x) =
    let ret = Array.copy node
    if level = 0 then 
        ret.[i &&& 0x01f] <- x :> obj 
    else
        let subidx = (i >>> level) &&& 0x01f
        ret.[subidx] <- doAssoc(level - 5, node.[subidx] :?> Node, i, x) :> obj
    ret

let assocN<'a> i (x:'a) (vector:PersistentVector<'a>) : PersistentVector<'a> =
    if i >= 0 && i < vector.Count then
        if i >= vector.TailOff then
            let newTail = Array.copy vector.Tail
            newTail.[i &&& 0x01f] <- x :> obj
            persistentVector(vector.Count, vector.Shift, vector.Root, newTail)
        else
            persistentVector(vector.Count, vector.Shift, doAssoc(vector.Shift, vector.Root, i, x), vector.Tail)
    elif i = vector.Count then
        cons x vector
    else raise Exceptions.OutOfBounds

let rangedIterator<'a> startIndex endIndex (vector:PersistentVector<'a>) : 'a seq =
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

let toSeq vector = rangedIterator 0 vector.Count vector