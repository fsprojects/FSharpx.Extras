/// A persistent vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentVector.java
module FSharpx.DataStructures.PersistentVector

open FSharpx

type Node(array: obj[])=
    new() = Node(Array.create 32 null)
    member this.Array = array

let emptyNode = Node()

type PersistentVector<'a> =  {
    Count : int
    Shift : int
    Root : Node
    Tail : obj[]
    TailOff: int }

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
    let ret = Node()
    ret.Array.[0] <- newPath(level - 5,node) :> obj
    ret

let rec pushTail(vector,level,parent:Node,tailnode) =
    //if parent is leaf, insert node,
    // else does it map to an existing child? -> nodeToInsert = pushNode one more level
    // else alloc new path
    //return  nodeToInsert placed in copy of parent
    let subidx = ((vector.Count - 1) >>> level) &&& 0x01f
    let ret = Node(Array.copy parent.Array)

    let nodeToInsert =
        if level = 5 then tailnode else

        let child = parent.Array.[subidx]
        if child <> null then
            pushTail(vector,level-5,child :?> Node,tailnode)
        else
            newPath(level-5,tailnode)

    ret.Array.[subidx] <- nodeToInsert :> obj
    ret

let inline arrayFor<'a> i (vector:PersistentVector<'a>) =
    if i >= 0 && i < vector.Count then
        if i >= vector.TailOff then vector.Tail else
            let node = ref vector.Root
            let level = ref vector.Shift
            while !level > 0 do
                let pos = (i >>> !level) &&& 0x01f
                node := (!node).Array.[pos] :?> Node
                level := !level - 5

            (!node).Array
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
        let tailNode = Node(vector.Tail)
        let newShift = vector.Shift

        //overflow root?
        if (vector.Count >>> 5) > (1 <<< vector.Shift) then
            let newRoot = Node()
            newRoot.Array.[0] <- vector.Root :> obj
            newRoot.Array.[1] <- newPath(vector.Shift,tailNode) :> obj
            persistentVector<'a>(vector.Count + 1,vector.Shift + 5,newRoot,[| x |])
        else
            let newRoot = pushTail(vector,vector.Shift,vector.Root,tailNode)
            persistentVector<'a>(vector.Count + 1,vector.Shift,newRoot,[| x |])


let rec doAssoc(level,node:Node,i,x) =
    let ret = Node(Array.copy node.Array)
    if level = 0 then 
        ret.Array.[i &&& 0x01f] <- x :> obj 
    else
        let subidx = (i >>> level) &&& 0x01f
        ret.Array.[subidx] <- doAssoc(level - 5, node.Array.[subidx] :?> Node, i, x) :> obj
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