module FSharpx.DataStructures.Vector

open FSharpx

type Node = obj[]

type TransientVector<'a> (count,shift:int,root:Node,tail:obj[]) =
    let mutable count = count
    let mutable root = root
    let mutable tail = tail
    let mutable shift = shift
    
    with
        member internal this.NewPath(level,node:Node) =
            if level = 0 then node else
            let ret = Array.create 32 null
            ret.[0] <- this.NewPath(level - 5,node) :> obj
            ret

        member internal this.PushTail(level,parent:Node,tailnode) =
            //if parent is leaf, insert node,
            // else does it map to an existing child? -> nodeToInsert = pushNode one more level
            // else alloc new path
            //return  nodeToInsert placed in copy of parent
    
            let subidx = ((count - 1) >>> level) &&& 0x01f
            let ret = Array.copy parent

            let nodeToInsert =
                if level = 5 then tailnode else

                let child = parent.[subidx]
                if child <> null then
                    this.PushTail(level-5,child :?> Node,tailnode)
                else
                    this.NewPath(level-5,tailnode)

            ret.[subidx] <- nodeToInsert :> obj
            ret

        member internal this.ArrayFor<'a> i =
            if i >= 0 && i < count then
                if i >= this.TailOff() then tail else
                    let mutable node = root
                    let mutable level = shift
                    while level > 0 do
                        let pos = (i >>> level) &&& 0x01f
                        node <- node.[pos] :?> Node
                        level <- level - 5

                    node
            else raise Exceptions.OutOfBounds

        member this.nth i =
                this.EnsureEditable()
                let node = this.ArrayFor i
                node.[i &&& 0x01f] :?> 'a

        member this.conj<'a> (x:'a) =
            this.EnsureEditable()

            //room in tail?
            if count - this.TailOff() < 32 then
                tail.[count &&& 0x01f] <- x :> obj
            else
                //full tail, push into tree
                let tailNode = tail
                let newShift = shift
                let newTail = Array.create 32 null
                newTail.[0] <- x :> obj

                //overflow root?
                let newRoot = 
                    if (count >>> 5) > (1 <<< shift) then
                        let newRoot = Array.create 32 null
                        newRoot.[0] <- root :> obj
                        newRoot.[1] <- this.NewPath(shift,tailNode) :> obj
                        shift <- shift + 5
                        newRoot
                    else
                        this.PushTail(shift,root,tailNode)

                tail <- newTail
                root <- newRoot

            count <- count + 1
            this

        member internal this.doAssoc(level,node:Node,i,x) =
            let ret = node
            if level = 0 then 
                ret.[i &&& 0x01f] <- x :> obj 
            else
                let subidx = (i >>> level) &&& 0x01f
                ret.[subidx] <- this.doAssoc(level - 5, node.[subidx] :?> Node, i, x) :> obj
            ret

        member this.assocN<'a> i (x:'a) : TransientVector<'a> =
            this.EnsureEditable()
            if i >= 0 && i < count then
                if i >= this.TailOff() then
                    tail.[i &&& 0x01f] <- x :> obj
                    this
                else
                    root <- this.doAssoc(shift, root, i, x)
                    this
            elif i = count then
                this.conj x
            else raise Exceptions.OutOfBounds

        member this.rangedIterator<'a>(startIndex,endIndex) : 'a seq =
            let i = ref startIndex
            let b = ref (!i - (!i % 32))
            let array = if startIndex < count then ref (this.ArrayFor !i) else ref null

            seq {
                while !i < endIndex do
                    if !i - !b = 32 then
                        array := this.ArrayFor !i
                        b := !b + 32

                    yield (!array).[!i &&& 0x01f] :?> 'a
                    i := !i + 1 
               }

        member this.persistent() =
            this.EnsureEditable()
            // TODO: root.edit.set(null)
            let l = count - this.TailOff()
            let trimmedTail = Array.init l (fun i -> tail.[i])
            PersistentVector(count, shift, root, trimmedTail)

        member internal this.EnsureEditable() = () // TODO:
        member this.Count = this.EnsureEditable(); count
        member internal this.TailOff() =
            if count < 32 then 0 else
            ((count - 1) >>> 5) <<< 5
        
        interface System.Collections.Generic.IEnumerable<'a> with
            member this.GetEnumerator () =
              this.rangedIterator(0,count).GetEnumerator()

        interface System.Collections.IEnumerable with
            member this.GetEnumerator () =
              (this.rangedIterator(0,count).GetEnumerator())
                :> System.Collections.IEnumerator

and PersistentVector<'a> (count,shift:int,root:Node,tail:obj[]) = 
    let tailOff = 
        if count < 32 then 0 else
        ((count - 1) >>> 5) <<< 5
    with
        member this.Count = count
        member internal this.Shift = shift
        member internal this.Root = root
        member internal this.Tail = tail
        member internal this.TailOff = tailOff