/// vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Vector.java

namespace FSharpx.Collections

module internal Literals = 

    [<Literal>]
    let internal blockSizeShift = 5 // TODO: what can we do in 64Bit case?

    [<Literal>]
    let internal blockSize = 32

    [<Literal>]
    let internal blockIndexMask = 0x01f

open FSharpx
open System.Threading

type Node(thread,array:obj[]) =
    let thread = thread
    new() = Node(ref null,Array.create Literals.blockSize null)
    with
        static member InCurrentThread() = Node(ref Thread.CurrentThread,Array.create Literals.blockSize null)
        member this.Array = array
        member this.Thread = thread
        member this.SetThread t = thread := t

type TransientVector<'T when 'T : equality> (count,shift:int,root:Node,tail:obj[]) =
    let mutable count = count
    let mutable root = root
    let mutable tail = tail
    let mutable shift = shift

    new() = TransientVector<'T>(0,Literals.blockSizeShift,Node.InCurrentThread(),Array.create Literals.blockSize null)
    
    member internal this.EnsureEditable(node:Node) =
        if node.Thread = root.Thread then node else
        Node(root.Thread,Array.copy node.Array)

    member internal this.NewPath(level,node:Node) =
        if level = 0 then node else
        let ret = Array.create Literals.blockSize null
        ret.[0] <- this.NewPath(level - Literals.blockSizeShift,node) :> obj
        Node(node.Thread,ret)

    member internal this.PushTail(level,parent:Node,tailnode) =
        //if parent is leaf, insert node,
        // else does it map to an existing child? -> nodeToInsert = pushNode one more level
        // else alloc new path
        //return  nodeToInsert placed in copy of parent
        let parent = this.EnsureEditable parent
        let subidx = ((count - 1) >>> level) &&& Literals.blockIndexMask
        let ret = parent

        let nodeToInsert =
            if level = Literals.blockSizeShift then tailnode else

            let child = parent.Array.[subidx]
            if child <> null then
                this.PushTail(level-Literals.blockSizeShift,child :?> Node,tailnode)
            else
                this.NewPath(level-Literals.blockSizeShift,tailnode)

        ret.Array.[subidx] <- nodeToInsert :> obj
        ret

    member internal this.ArrayFor i =
        if i >= 0 && i < count then
            if i >= this.TailOff() then tail else
                let mutable node = root
                let mutable level = shift
                while level > 0 do
                    let pos = (i >>> level) &&& Literals.blockIndexMask
                    node <- node.Array.[pos] :?> Node
                    level <- level - Literals.blockSizeShift

                node.Array
        else raise (new System.IndexOutOfRangeException())

    member this.conj<'T> (x:'T) =
        this.EnsureEditable()

        //room in tail?
        if count - this.TailOff() < Literals.blockSize then
            tail.[count &&& Literals.blockIndexMask] <- x :> obj
        else
            //full tail, push into tree
            let tailNode = Node(root.Thread,tail)
            let newShift = shift
            let newTail = Array.create Literals.blockSize null
            newTail.[0] <- x :> obj

            //overflow root?
            let newRoot = 
                if (count >>> Literals.blockSizeShift) > (1 <<< shift) then
                    let newRoot = Node(root.Thread,Array.create Literals.blockSize null)
                    newRoot.Array.[0] <- root :> obj
                    newRoot.Array.[1] <- this.NewPath(shift,tailNode) :> obj
                    shift <- shift + Literals.blockSizeShift
                    newRoot
                else
                    this.PushTail(shift,root,tailNode)

            tail <- newTail
            root <- newRoot

        count <- count + 1
        this

    member this.rangedIterator<'T>(startIndex,endIndex) : 'T seq =
        let i = ref startIndex
        let b = ref (!i - (!i % Literals.blockSize))
        let array = if startIndex < count then ref (this.ArrayFor !i) else ref null

        seq {
            while !i < endIndex do
                if !i - !b = Literals.blockSize then
                    array := this.ArrayFor !i
                    b := !b + Literals.blockSize

                yield (!array).[!i &&& Literals.blockIndexMask] :?> 'T
                i := !i + 1 
            }

    member this.persistent() : Vector<'T> =
        this.EnsureEditable()
        root.SetThread null
        let l = count - this.TailOff()
        let trimmedTail = Array.init l (fun i -> tail.[i])
        Vector(count, shift, root, trimmedTail)

    member internal this.EnsureEditable() =
        if !root.Thread = Thread.CurrentThread then () else
        if !root.Thread <> null then
            failwith "Transient used by non-owner thread"
        failwith "Transient used after persistent! call"

    member internal this.TailOff() =
        if count < Literals.blockSize then 0 else
        ((count - 1) >>> Literals.blockSizeShift) <<< Literals.blockSizeShift
        
    interface System.Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator () =
          this.rangedIterator(0,count).GetEnumerator()

    interface System.Collections.IEnumerable with
        member this.GetEnumerator () =
          (this.rangedIterator(0,count).GetEnumerator())
            :> System.Collections.IEnumerator 

and Vector<[<EqualityConditionalOn>]'T when 'T : equality> (count,shift:int,root:Node,tail:obj[])  =
    let hashCode = ref None
    let tailOff = 
        if count < Literals.blockSize then 0 else
        ((count - 1) >>> Literals.blockSizeShift) <<< Literals.blockSizeShift

    static member Empty() : Vector<'T> = Vector<'T>(0,Literals.blockSizeShift,Node(),[||])

    static member ofSeq(items:'T seq) =
        let mutable ret = TransientVector()
        for item in items do
            ret <- ret.conj item
        ret.persistent()

    override this.GetHashCode() =
        match !hashCode with
        | None ->
            let mutable hash = 1
            for x in this.rangedIterator(0,count) do
                hash <- 31 * hash + Unchecked.hash x
            hashCode := Some hash
            hash
        | Some hash -> hash

    override this.Equals(other) =
        match other with
        | :? Vector<'T> as y -> 
            if this.Length <> y.Length then false else
            if this.GetHashCode() <> y.GetHashCode() then false else
            Seq.forall2 (Unchecked.equals) this y
        | _ -> false

    member internal this.SetHash hash = hashCode := hash; this

    member internal this.NewPath(level,node:Node) =
        if level = 0 then node else
        let ret = Node(root.Thread,Array.create Literals.blockSize null)
        ret.Array.[0] <- this.NewPath(level - Literals.blockSizeShift,node) :> obj
        ret

    member internal this.PushTail(level,parent:Node,tailnode) =
        //if parent is leaf, insert node,
        // else does it map to an existing child? -> nodeToInsert = pushNode one more level
        // else alloc new path
        //return  nodeToInsert placed in copy of parent
        let subidx = ((count - 1) >>> level) &&& Literals.blockIndexMask
        let ret = Node(parent.Thread,Array.copy parent.Array)

        let nodeToInsert =
            if level = Literals.blockSizeShift then tailnode else

            let child = parent.Array.[subidx]
            if child <> null then
                this.PushTail(level-Literals.blockSizeShift,child :?> Node,tailnode)
            else
                this.NewPath(level-Literals.blockSizeShift,tailnode)

        ret.Array.[subidx] <- nodeToInsert :> obj
        ret

    member internal this.ArrayFor i =
        if i >= 0 && i < count then
            if i >= tailOff then tail else
                let mutable node = root
                let mutable level = shift
                while level > 0 do
                    let pos = (i >>> level) &&& Literals.blockIndexMask
                    node <- node.Array.[pos] :?> Node
                    level <- level - Literals.blockSizeShift

                node.Array
        else raise (System.IndexOutOfRangeException())

    member internal this.doAssoc(level,node:Node,i,x) =
        let ret = Node(root.Thread,Array.copy node.Array)
        if level = 0 then 
            ret.Array.[i &&& Literals.blockIndexMask] <- x :> obj 
        else
            let subidx = (i >>> level) &&& Literals.blockIndexMask
            ret.Array.[subidx] <- this.doAssoc(level - Literals.blockSizeShift, node.Array.[subidx] :?> Node, i, x) :> obj
        ret

    member internal this.PopTail(level,node:Node) : Node =
        let subidx = ((count-2) >>> level) &&& Literals.blockIndexMask
        if level > Literals.blockSizeShift then
            let newchild = this.PopTail(level - Literals.blockSizeShift, node.Array.[subidx] :?> Node)
            if newchild = Unchecked.defaultof<Node> && subidx = 0 then Unchecked.defaultof<Node> else
            let ret = Node(root.Thread, Array.copy node.Array);
            ret.Array.[subidx] <- newchild  :> obj
            ret

        elif subidx = 0 then Unchecked.defaultof<Node> else

        let ret = new Node(root.Thread, Array.copy node.Array)
        ret.Array.[subidx] <- null
        ret

    member this.rangedIterator<'T>(startIndex,endIndex) : 'T seq =
        let i = ref startIndex
        let b = ref (!i - (!i % Literals.blockSize))
        let array = if startIndex < count then ref (this.ArrayFor !i) else ref null

        seq {
            while !i < endIndex do
                if !i - !b = Literals.blockSize then
                    array := this.ArrayFor !i
                    b := !b + Literals.blockSize

                yield (!array).[!i &&& Literals.blockIndexMask] :?> 'T
                i := !i + 1 
            }
        
    member this.Conj (x : 'T) = 
        if count - tailOff < Literals.blockSize then
            let newTail = Array.append tail [|x:>obj|]
            Vector<'T>(count + 1,shift,root,newTail) 
        else
            //full tail, push into tree
            let tailNode = Node(root.Thread,tail)
            let newShift = shift

            //overflow root?
            if (count >>> Literals.blockSizeShift) > (1 <<< shift) then
                let newRoot = Node()
                newRoot.Array.[0] <- root :> obj
                newRoot.Array.[1] <- this.NewPath(shift,tailNode) :> obj
                Vector<'T>(count + 1,shift + Literals.blockSizeShift,newRoot,[| x |])
            else
                let newRoot = this.PushTail(shift,root,tailNode)
                Vector<'T>(count + 1,shift,newRoot,[| x |])

    member this.Initial =
        if count = 0 then failwith "Can't initial empty vector" else
        if count = 1 then Vector<'T>.Empty() else

        if count - tailOff > 1 then 
            let mutable newroot = Node(ref Thread.CurrentThread, root.Array.Clone() :?> obj[])
            let mutable ret = TransientVector(count - 1, shift, newroot, tail.[0..(tail.Length-1)])
            ret.persistent() 
        else
            let newtail = this.ArrayFor(count - 2)

            let mutable newroot = this.PopTail(shift, root)
            let mutable newshift = shift
            if newroot = Unchecked.defaultof<Node> then
                newroot <- Node()

            if shift > Literals.blockSizeShift && newroot.Array.[1] = null then
                newroot <- newroot.Array.[0] :?> Node
                newshift <- newshift - Literals.blockSizeShift

            Vector(count - 1, newshift, newroot, newtail)

    member this.TryInitial = if count = 0 then None else Some(this.Initial)

    member this.IsEmpty = (count = 0)

    member this.Item 
        with get i = 
            let node = this.ArrayFor i
            node.[i &&& Literals.blockIndexMask] :?> 'T

    member this.Last = if count > 0 then this.[count - 1] else failwith "Can't peek empty vector"

    member this.TryLast = if count > 0 then Some (this.[count - 1]) else None

    member this.Length : int = count

    member this.Rev() =
        if count = 0 then Vector.Empty() :> Vector<'T>
        else
            let i = ref (count - 1)
            let array = ref (this.ArrayFor !i)

            let items = seq {
                while !i > - 1 do
                    if (!i + 1) % Literals.blockSize  = 0 then
                        array := this.ArrayFor !i

                    yield (!array).[!i &&& Literals.blockIndexMask] :?> 'T
                    i := !i - 1 
                }

            let mutable ret = TransientVector()

            for item in items do
                ret <- ret.conj item

            ret.persistent()

    member this.Unconj = if count > 0 then this.Initial, this.[count - 1] else failwith "Can't peek empty vector"

    member this.TryUnconj = if count > 0 then Some(this.Initial, this.[count - 1])  else None

    member this.Update(i, x : 'T) = 
        if i >= 0 && i < count then
            if i >= tailOff then
                let newTail = Array.copy tail
                newTail.[i &&& Literals.blockIndexMask] <- x :> obj
                Vector(count, shift, root, newTail)
            else
                Vector(count, shift, this.doAssoc(shift, root, i, x),tail)
        elif i = count then this.Conj x 
        else raise (new System.IndexOutOfRangeException())

    member this.TryUpdate(i, x : 'T) =
        if i >= 0 && i < count then Some(this.Update (i,x))
        else None

    interface System.Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator () =
          this.rangedIterator(0,count).GetEnumerator()

    interface System.Collections.IEnumerable with
        member this.GetEnumerator () =
          (this.rangedIterator(0,count).GetEnumerator())
            :> System.Collections.IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector = 
    //pattern discriminators  (active pattern)
    let (|Conj|Nil|) (v : Vector<'T>) = match v.TryUnconj with Some(a,b) -> Conj(a,b) | None -> Nil
     
    let append (vectorA : Vector<'T>) (vectorB : Vector<'T>) = 
        let mutable ret = TransientVector()
        for i in 0..(vectorA.Length - 1) do
            ret <- ret.conj vectorA.[i]
        for i in 0..(vectorB.Length - 1) do
            ret <- ret.conj vectorB.[i]
        ret.persistent() 

    let inline conj (x : 'T) (vector : Vector<'T>) = vector.Conj x

    let empty<'T when 'T : equality> = Vector.Empty() :> Vector<'T>

    let inline fold (f : ('State -> 'T -> 'State)) (state : 'State) (v : Vector<'T>) = 
        let rec loop state' (v' : Vector<'T>) count =
            match count with
            | _ when count = v'.Length -> state'
            | _ -> loop (f state' v'.[count]) v' (count + 1)  
        loop state v 0

    let inline flatten (v : Vector<Vector<'T>>) =
        fold (fun (s : seq<'T>) (v' : Vector<'T>) -> Seq.append s v') Seq.empty<'T> v

    let inline foldBack (f : ('T -> 'State -> 'State)) (v : Vector<'T>) (state : 'State) =  
        let rec loop state' (v' : Vector<'T>) count =
            match count with
            | -1 -> state'
            | _ -> loop (f v'.[count] state') v' (count - 1)  
        loop state v (v.Length - 1)

    let init count (f: int -> 'T) : Vector<'T> =
        let mutable ret = TransientVector()
        for i in 0..(count-1) do
            ret <- ret.conj(f i)
        ret.persistent() 

    let inline initial (vector: Vector<'T>) = vector.Initial

    let inline tryInitial (vector: Vector<'T>) = vector.TryInitial

    let inline isEmpty (vector: Vector<'T>) = vector.IsEmpty

    let inline last (vector: Vector<'T>) = vector.Last

    let inline tryLast (vector: Vector<'T>) = vector.TryLast

    let inline length (vector: Vector<'T>) : int = vector.Length

    let map (f : 'T -> 'T1) (vector: Vector<'T>) : 'T1 Vector = 
        let mutable ret = TransientVector()
        for item in vector do
            ret <- ret.conj(f item)
        ret.persistent() 

    let inline nth i (vector: Vector<'T>) : 'T = vector.[i]

    let inline nthNth i j (vector: Vector<Vector<'T>>) : 'T = vector.[i] |> nth j
 
    let inline tryNth i (vector: Vector<'T>) =
        if i >= 0 && i < vector.Length then Some(vector.[i])
        else None

    let inline tryNthNth i j (vector: Vector<Vector<'T>>) =
        match tryNth i vector with
        | Some v' -> tryNth j v'
        | None -> None    

    let ofSeq (items : 'T seq) = Vector.ofSeq items 

    let inline rev (vector: Vector<'T>) = vector.Rev()

    let inline singleton (x : 'T) = empty |> conj x

    let inline toSeq (vector: Vector<'T>) = vector :> seq<'T>

    let inline unconj (vector: Vector<'T>) = vector.Unconj

    let inline tryUnconj (vector: Vector<'T>) = vector.TryUnconj

    let inline update i (x : 'T) (vector : Vector<'T>) : Vector<'T> = vector.Update(i, x)

    let inline updateNth i j (x : 'T) (vector : Vector<Vector<'T>>) : Vector<Vector<'T>> = vector.Update(i, (vector.[i].Update(j, x)))

    let inline tryUpdate i (x : 'T) (vector : Vector<'T>) = vector.TryUpdate(i, x)

    let inline tryUpdateNth i  j (x : 'T) (vector : Vector<Vector<'T>>) = 
        if i >= 0 && i < vector.Length && j >= 0 && j < vector.[i].Length
        then Some(updateNth i j x vector)
        else None

    let inline windowFun windowLength = 
        fun (v : Vector<Vector<'T>>) x ->
        if v.Last.Length = windowLength 
        then 
            v 
            |> conj (empty.Conj(x))
        else 
            initial v 
            |> conj (last v |> conj x)

    let inline windowSeq windowLength (items : 'T seq) = 
        if windowLength < 1 then invalidArg "windowLength" "length is less than 1"
        else (Seq.fold (windowFun windowLength) (empty.Conj empty<'T>) items)