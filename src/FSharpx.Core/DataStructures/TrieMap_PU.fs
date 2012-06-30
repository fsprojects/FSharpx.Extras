// ----------------------------------------------------------------------------
// F# TrieMap implementation (TrieMap.fs)
// (c) Matthew Lamari, 2012, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

namespace FSharpx.DataStructures.TrieMap_PU

open FSharpx
open System
open System.Collections
open System.Collections.Generic


// Trie Hash map.  Built for speed, not comfort - discarded a lot of static checks for ability to use null (and direct type checks).
// Even mutability on the nodes (to pass them around pre-placement)
// API *should* conceal these implementation details.

// Key doesn't have to be comparable like Map; but has to support equality
// Tends to get a speed advantage over Map when comparison has a cost, or the table gets huge with more additions
// than replacements (replacements don't push heavy rebalancing onto Map's tree)

// Current TrieMap implementation is as a struct of 2 elements, trying limiting allocation on the outer node creation.

module TMUtil =
    let getHashCode x = x.GetHashCode()
    let bucketCountShift = 4 // this seems the sweet spot for performance, 16 (2 ^ 4) buckets
    let shiftAtWhichYouHitBottom = (32 / bucketCountShift) * bucketCountShift
    let bucketCount = 1 <<< bucketCountShift
    let omBucketCount = bucketCount - 1
    let bucketMask = (1 <<< bucketCountShift) - 1

    let byteToBitCount =
        Array.init
            256
            (fun n ->
                let mutable current = n &&& 255
                let mutable count = 0
                while current <> 0 do
                    if (current &&& 1) = 1 then count <- count + 1
                    current <- current >>> 1
                count)

    let inline getBitCount bits =
        let mutable count = 0
        let mutable current = bits
        while current <> 0 do
            count <- count + (byteToBitCount.[current &&& 255])
            current <- current >>> 8
        count

    let maskForBeforeLogicalIndex = (Array.init 32 (fun n -> if n = 0 then 0 else (int) (0xffffffffu >>> (32 - n))))
    let boxedNumbers = Array.init (1 <<< bucketCount) (fun n -> box n)

    let inline singleElementArrayCreateFromSingleBitMask (singleBitMask : int) (element : Object) : Object array =
        let arr = Array.create 2 null
        arr.[0] <- boxedNumbers.[singleBitMask]
        arr.[1] <- element
        arr

    let inline singleElementArrayCreateFromLogicalIndex (logicalIndex : int) (element : Object) : Object array =
        singleElementArrayCreateFromSingleBitMask (1 <<< logicalIndex) element

    let inline dualElementArrayCreate (logicalIndex0 : int) element0 (logicalIndex1 : int) element1 : Object array =
        let arr = Array.create 3 null
        arr.[0] <- boxedNumbers.[(1 <<< logicalIndex0) ||| (1 <<< logicalIndex1)]
        if (logicalIndex0 < logicalIndex1) then
            arr.[1] <- element0 :> Object
            arr.[2] <- element1 :> Object
        else
            arr.[1] <- element1 :> Object
            arr.[2] <- element0 :> Object
        arr

    type ArrayTargetData =
        struct
            //val OriginalArray : Object array
            val Bits : int
            val NumBefore : int
            val SomethingAlreadyPresent : Object
            new (bits, numBefore, somethingAlreadyPresent)  = { Bits = bits; NumBefore = numBefore; SomethingAlreadyPresent = somethingAlreadyPresent }
        end

    // used for efficient replacement
    let inline getArrayTargetData (originalArray : Object array) (logicalIndex : int) : ArrayTargetData =
        let (bits : int) = unbox originalArray.[0]
        let logicalIndexMask = 1 <<< logicalIndex
        let numBefore = getBitCount (bits &&& maskForBeforeLogicalIndex.[logicalIndex])
        new ArrayTargetData(
            bits,
            numBefore,
            if (bits &&& logicalIndexMask) <> 0 then
                originalArray.[1 + numBefore]
            else null)

    let inline arrayWithLogicalInsertion (arrayTargetData : ArrayTargetData) (originalArray : Object array) (insertionPointLogicalIndex : int) (element : Object) : Object array =
        let cloned = Array.create (originalArray.Length + 1) null
        let logicalIndexMask = 1 <<< insertionPointLogicalIndex
        cloned.[0] <- boxedNumbers.[arrayTargetData.Bits ||| logicalIndexMask]
        let numBefore = arrayTargetData.NumBefore
        Array.Copy(originalArray, 1, cloned, 1, numBefore)
        cloned.[numBefore + 1] <- element
        Array.Copy(originalArray, numBefore + 1, cloned, numBefore + 2, originalArray.Length - numBefore - 1)
        cloned

    let inline arrayWithLogicalRemoval (arrayTargetData : ArrayTargetData) (originalArray : Object array) (removalPointLogicalIndex : int) : Object array =
        let cloned = Array.create (originalArray.Length - 1) null
        let logicalIndexMask = 1 <<< removalPointLogicalIndex
        cloned.[0] <- boxedNumbers.[arrayTargetData.Bits - logicalIndexMask]
        let numBefore = arrayTargetData.NumBefore
        Array.Copy(originalArray, 1, cloned, 1, numBefore)
        Array.Copy(originalArray, numBefore + 2, cloned, numBefore + 1, originalArray.Length - numBefore - 2)
        cloned

    // an optimization that we return the info rather than take a closure.
    type ReplacementArrayInfo =
        struct
            val NewArray : Object array
            val TargetPhysicalIndex : int
            val ExistingElement : Object
            new (newArray : Object array, targetPhysicalIndex, existingElement) = { NewArray = newArray; TargetPhysicalIndex = targetPhysicalIndex; ExistingElement = existingElement }
        end

    let inline getReplacementInfo (arrayTargetData : ArrayTargetData) (originalArray : Object array) : ReplacementArrayInfo =
        let physicalIndex = arrayTargetData.NumBefore + 1 // the + 1 is to get us beyond the bit-map
        new ReplacementArrayInfo(Array.copy originalArray, physicalIndex, originalArray.[physicalIndex])

    let inline getOriginalArrayElementCount (originalArray : Object array) : int =
        originalArray.Length - 1




type TrieMap<'Key, 'T when 'Key : equality> =
    struct
        val private count : int
        val private rootNode : Object
        private new(count : int, rootNode : Object) = { count = count; rootNode = rootNode }
    end


    // flag returns True if there was a net new node added, false if (overwrite) keeps count constant
    // can return a new node OR an array
    static member inline private settleCollision(originalNode : HKVNode<'Key, 'T>) (newNode : HKVNode<'Key, 'T>) bucketShift : Flagged<Object> =
        if newNode.Key = originalNode.Key then // this takes care of the singleton on singleton AND the head of the list
            newNode.Next <- originalNode.Next
            new Flagged<Object>(newNode :> Object, false)
        else
            let rec run originalNode newNode bucketShift = // bit of a speed boost here, can inline outer function, and not repeat the first (check for equal) clause.
                if bucketShift = TMUtil.shiftAtWhichYouHitBottom then
                    // hit bottom - sort it out, find if already exists and rebuild accordingly

                    // going for speed here
                    let mutable amDone = false
                    let mutable wasFound = false
                    let mutable (afterFound : HKVNode<'Key, 'T>) = originalNode // whatever - can't nicely assign it to null, we depend on the flag WasFound
                    let mutable current = originalNode.Next // remember - we've checked for the match of hkv being at the TOP at the beginning of the outer function
                    if Object.ReferenceEquals(current, null) then
                        amDone <- true
                    while not amDone do
                        if current.Key = newNode.Key then
                            // found own key
                            wasFound <- true
                            afterFound <- current.Next
                            amDone <- true
                        else
                            if System.Object.ReferenceEquals(current.Next, null) then
                                amDone <- true
                            else current <- current.Next

                    match wasFound with
                    | false->
                        newNode.Next <- originalNode
                        new Flagged<Object>(newNode :> Object, true)
                    | true -> // found ourselves, have to overwrite.  Trust afterFound now.
                        newNode.Next <- afterFound
                        let mutable rl = newNode
                        let mutable current = originalNode
                        while (not (current.Key = newNode.Key)) do
                            rl <- { current with Next = rl }
                            current <- current.Next
                        new Flagged<Object>(rl :> Object, false) // no net add
                else
                    let originalsLogicalIndex = (originalNode.Hash >>> bucketShift) &&& TMUtil.bucketMask
                    let newItemLogicalIndex = (newNode.Hash >>> bucketShift) &&& TMUtil.bucketMask
                    if originalsLogicalIndex = newItemLogicalIndex then
                        // they've collided, recurse
                        let result = run originalNode newNode (bucketShift + TMUtil.bucketCountShift)
                        new Flagged<Object>(TMUtil.singleElementArrayCreateFromLogicalIndex originalsLogicalIndex result.Value, result.Flag)
                    else
                        // they can coexist in the new array
                        new Flagged<Object>(TMUtil.dualElementArrayCreate originalsLogicalIndex originalNode newItemLogicalIndex newNode, true)
            run originalNode newNode bucketShift

    // flag returns True if there was a net new node added, false if (overwrite) keeps count constant
    static member inline private augmentedArray(originalArray : Object array) (newNode : HKVNode<'Key, 'T>) bucketShift : Flagged<Object array> =
        let rec run (originalArray : Object array) newNode bucketShift = // this inner recursive, plus the inline on the outer that it allowed, gave a speed boost
            let logicalIndex = (newNode.Hash >>> bucketShift) &&& TMUtil.bucketMask
            let arrayTargetData = TMUtil.getArrayTargetData originalArray logicalIndex
            match arrayTargetData.SomethingAlreadyPresent with
            | null -> new Flagged<Object array>(TMUtil.arrayWithLogicalInsertion arrayTargetData originalArray logicalIndex newNode, true)
            | elt ->
                let replacementArrayInfo = TMUtil.getReplacementInfo arrayTargetData originalArray
                // in attempted order of likelihood
                match replacementArrayInfo.ExistingElement with
                | :? (Object array) as subArray ->
                    let result = run subArray newNode (bucketShift + TMUtil.bucketCountShift)
                    replacementArrayInfo.NewArray.[replacementArrayInfo.TargetPhysicalIndex] <- result.Value :> Object
                    new Flagged<Object array>(replacementArrayInfo.NewArray, result.Flag)
                | :? HKVNode<'Key, 'T> as node ->
                    let result = TrieMap<'Key, 'T>.settleCollision node newNode (bucketShift + TMUtil.bucketCountShift)
                    replacementArrayInfo.NewArray.[replacementArrayInfo.TargetPhysicalIndex] <- result.Value
                    new Flagged<Object array>(replacementArrayInfo.NewArray, result.Flag)
                | _ -> failwith "unknown type"
        run originalArray newNode bucketShift

    member inline private this.withAddition(k : 'Key, v : 'T) : TrieMap<'Key, 'T> =
        let h = TMUtil.getHashCode k
        //let hkv = new HKV<'Key, 'T>(k, v, h)
        let newNode = { Key = k; Value = v; Hash = h; Next = Unchecked.defaultof<HKVNode<'Key, 'T>> }
        match this.rootNode with
        | :? (Object array) as arr ->
            let result = TrieMap.augmentedArray arr newNode 0
            TrieMap((if result.Flag then this.count + 1 else this.count), result.Value)
        | null -> // first node
            new TrieMap<'Key, 'T>(1, (newNode :> Object))
        | :? HKVNode<'Key, 'T> as node ->
            let result = TrieMap.settleCollision node newNode 0
            TrieMap((if result.Flag then this.count + 1 else this.count), result.Value)
        | _ -> failwith "unknown type"

    member inline private this.findInTHash (k : 'Key) : 'T option =
        let rec find k (keyHash : int) (bucketShift : int) (node : Object) = // deliberately don't let null in
            match node with
            | :? HKVNode<'Key, 'T> as node ->
                let mutable found = false
                let mutable foundAt = node.Next
                if node.Key = k then
                    Some node.Value
                else
                    let mutable current = node.Next
                    while (not found) && (not (Object.ReferenceEquals(current, null))) do
                        if current.Key = k then
                            found <- true
                            foundAt <- current
                        else
                            current <- current.Next
                    if found then Some current.Value else None
            | :? (Object array) as arr ->
                let logicalIndex = (keyHash >>> bucketShift) &&& TMUtil.bucketMask
                let arrayTargetData = TMUtil.getArrayTargetData arr logicalIndex
                match (arrayTargetData.SomethingAlreadyPresent) with
                | null -> None
                | item ->
                    //let foo = TMUtil.maskForBeforeLogicalIndex
                    //let physicalIndex = arrayTargetData.NumBefore + 1
                    //let item = arr.[physicalIndex]
                    find k keyHash (bucketShift + TMUtil.bucketCountShift) item
            | _ -> failwith "Unknown type on find"
        match this.rootNode with
        | null -> None
        | item -> find k (TMUtil.getHashCode k) 0 item

    static member inline private deleteFromNodeList nodeList (k : 'Key) : Flagged<Object> =
        let mutable found = false
        let mutable (afterFound : HKVNode<'Key, 'T>) = nodeList // whatever - can't nicely assign it to null, we depend on the flag WasFound
        let mutable current = nodeList
        let mutable amDone = false
        while not amDone do
            if current.Key = k then
                afterFound <- current.Next
                amDone <- true
                found <- true
            else
                if System.Object.ReferenceEquals(current.Next, null) then
                    amDone <- true
                else
                    current <- current.Next
        if found then
            let mutable rl = afterFound
            let mutable current = nodeList
            while (not (current.Key = k)) do
                rl <- { current with Next = rl }
                current <- current.Next
            new Flagged<Object>(rl :> Object, true)
        else
            new Flagged<Object>(nodeList, false)



    static member inline private getArrayDeleteResultWithIndexRemoved(originalArray : Object array) (logicalIndex : int) =
        let originalArrayElementCount = TMUtil.getOriginalArrayElementCount originalArray
        if (originalArrayElementCount = 1) then
            new Flagged<Object>(null, true) // deletion produced nothing, and it was the only thing in the array.
        else
            let arrayTargetData = TMUtil.getArrayTargetData originalArray logicalIndex
            if originalArrayElementCount = 2 then // for the case when there's only one other thing; but it's a Node not an array
                // this could be a candidate for factoring, as it assumes details about the array implementation
                let bitMaskOfOther = arrayTargetData.Bits - (1 <<< logicalIndex)
                let otherInArray = if arrayTargetData.NumBefore > 0 then originalArray.[1] else originalArray.[2] // this works because there are only 2
                match otherInArray with
                | :? (HKVNode<'Key, 'T>) as node when System.Object.ReferenceEquals(node.Next, null) ->
                        new Flagged<Object>(node, true) // return the node instead of the array, proliferate a loner back up the chain
                | _ -> // must be a sub-array, or multiple nodes at bottom level, either being there for a reason for a reason - keep this an array.
                    new Flagged<Object>(TMUtil.singleElementArrayCreateFromSingleBitMask bitMaskOfOther otherInArray, true)
            else // > 1 other item in array, so we copy the array up MINUS index = index
                new Flagged<Object>(TMUtil.arrayWithLogicalRemoval arrayTargetData originalArray logicalIndex, true)

    // returns true only if something was actually deleted
    static member private deletedFromArray(originalArray : Object array) (k : 'Key) (h : int) bucketShift : Flagged<Object>  =
        let logicalIndex = (h >>> bucketShift) &&& TMUtil.bucketMask
        let arrayTargetData = TMUtil.getArrayTargetData originalArray logicalIndex

        match arrayTargetData.SomethingAlreadyPresent with
        | null -> new Flagged<Object>(originalArray, false)
        | :? (Object array) as subArray ->
            let result = TrieMap<'Key, 'T>.deletedFromArray subArray k h (bucketShift + TMUtil.bucketCountShift)
            if result.Flag then
                // there was a deletion.  Several cases.
                // If it's sending up a Node, it's a lone node
                // If it's null, we have to check if this results in this array being empty.
                match result.Value with
                | null -> // see if there's only one
                    match TMUtil.getOriginalArrayElementCount originalArray with
                    | 1 -> result // proliferate the true-delete-null-result array
                    | n ->
                        assert (n <> 0)
                        let newArray = TMUtil.arrayWithLogicalRemoval arrayTargetData originalArray logicalIndex
                        if n = 2 then
                            match newArray.[1] with // if result . .  here. . . would be a single-array with a single, proliferate it up.
                            | :? (HKVNode<'Key, 'T>) as node
                                when (match (node.Next :> Object) with | null -> true | _ -> false) -> new Flagged<Object>(node, true)
                            | _ -> new Flagged<Object>(newArray, true)
                        else
                            new Flagged<Object>(newArray, true)
                | :? (Object array) as newSubArray -> // pretty simple, sub this in, return as a delete
                    let replacementArrayInfo = TMUtil.getReplacementInfo arrayTargetData originalArray
                    replacementArrayInfo.NewArray.[replacementArrayInfo.TargetPhysicalIndex] <- newSubArray :> Object
                    new Flagged<Object>(replacementArrayInfo.NewArray, true)
                | :? (HKVNode<'Key, 'T>) as node -> // array delete returned a kvnode - only reason could be is if a lower list went to length of 1 and we're floating it up.
                    if (TMUtil.getOriginalArrayElementCount originalArray) = 1 then
                        new Flagged<Object>(node, true)
                    else
                        let replacementArrayInfo = TMUtil.getReplacementInfo arrayTargetData originalArray
                        replacementArrayInfo.NewArray.[replacementArrayInfo.TargetPhysicalIndex] <- node :> Object
                        new Flagged<Object>(replacementArrayInfo.NewArray, true)
                | _ -> failwith "unknown returned type"
            else
                new Flagged<Object>(originalArray, false)
        | :? HKVNode<'Key, 'T> as node ->
            let result = TrieMap<'Key, 'T>.deleteFromNodeList node k
            if result.Flag then // something was deleted
                match result.Value with
                | null -> TrieMap<'Key, 'T>.getArrayDeleteResultWithIndexRemoved originalArray logicalIndex
                | :? (HKVNode<'Key, 'T>) as node ->
                    if ((TMUtil.getOriginalArrayElementCount originalArray) = 1) && (System.Object.ReferenceEquals(node.Next, null)) then // solo solo - return it
                        new Flagged<Object>(node, true)
                    else
                        let replacementArrayInfo = TMUtil.getReplacementInfo arrayTargetData originalArray
                        replacementArrayInfo.NewArray.[replacementArrayInfo.TargetPhysicalIndex] <- node :> Object
                        new Flagged<Object>(replacementArrayInfo.NewArray, true)
                | _ -> failwith "Delete of a node shouldn't return anything but null or a node"
            else
                new Flagged<Object>(originalArray, false) // nothing deleted
        | _ -> failwith "Unknown type on delete from array"


    member inline private this.withRemoval(key : 'Key)  =
        match this.rootNode with
        | null -> TrieMap(0, Unchecked.defaultof<HKVNode<'Key, 'T>>) // already empty
        | :? (Object array) as arr ->
            let result = TrieMap<'Key, 'T>.deletedFromArray arr key (TMUtil.getHashCode key) 0
            if result.Flag then TrieMap(this.Count - 1, result.Value) else this
        | :? HKVNode<'Key, 'T> as node ->
            assert System.Object.ReferenceEquals(node.Next, null)
            if node.Key = key then TrieMap(0, null) else this
        | _ -> failwith "Unknown type on delete"

    member inline private this.getTHashKVPairs () : ('Key * 'T) seq =
        let rec getItems (node : Object) : ('Key * 'T) seq =
            match node with
            | null -> Seq.empty
            | :? HKVNode<'Key, 'T> as node ->
                seq { yield (node.Key, node.Value); yield! getItems node.Next }
            | :? (Object array) as arr ->
                arr |> Seq.skip 1 |> Seq.map getItems |> Seq.concat
            | _ -> failwith "unknown type"
        getItems this.rootNode



    // Public Map-style accessors
    member public this.Add((key : 'Key, value : 'T) as keyValue) : TrieMap<'Key, 'T> = this.withAddition keyValue
    member public this.ContainsKey(key : 'Key) : bool = match this.findInTHash(key) with | None -> false | Some _ -> true
    member public this.Count = this.count
    member public this.IsEmpty : bool = this.Count = 0
    member public this.Item(key : 'Key) : 'T = match this.findInTHash(key) with | None -> this.findInTHash(key) |> ignore; raise (KeyNotFoundException(key.ToString())) | Some value -> value
    member public this.Remove(key : 'Key) = this.withRemoval key
    member public this.TryFind(key : 'Key) : 'T option = this.findInTHash key

    static member Empty = new TrieMap<'Key, 'T>()

    interface IEnumerable<'Key * 'T> with
        member this.GetEnumerator() = (this.getTHashKVPairs()).GetEnumerator()
        member this.GetEnumerator() = (this.getTHashKVPairs()).GetEnumerator() :> IEnumerator

            

and
 private Flagged<'fv> =
    struct
        val Value : 'fv
        val Flag : bool
        new (value : 'fv, flag : bool) = { Value = value; Flag = flag }
    end
and
 private HKVNode<'Key, 'T when 'Key : equality> = {
    Key : 'Key
    Value : 'T
    Hash : int
    mutable Next : HKVNode<'Key, 'T> // this lets HKVNode perform double-duty, being mutated when put in place after being passed down a chain.  Never mutated once placed
    }




module TrieMap =
    // these attempt to mimic the behavior of the Map module equivalents

    let add key value (table : TrieMap<'Key, 'T>) = table.Add(key, value)
    let remove key (table : TrieMap<'Key, 'T>) = table.Remove key
    let containsKey key (table : TrieMap<'Key, 'T>) = table.ContainsKey
    let empty<'Key, 'T when 'Key : equality> : TrieMap<'Key, 'T> = TrieMap<'Key, 'T>.Empty //new TrieMap<'Key, 'T>()
    let find<'Key, 'T when 'Key : equality> key (table : TrieMap<'Key, 'T>) = table.[key]
    let tryFind<'Key, 'T when 'Key : equality> key (table : TrieMap<'Key, 'T>) = table.TryFind key

    // These need more optimization within the Trie, it's not too difficult; but would speed things up, to create the arrays one at a time
    // Currently implemented in terms of a fold
    let ofSeq<'Key, 'T when 'Key : equality> (elements : ('Key * 'T) seq) = elements |> Seq.fold (fun (table : TrieMap<'Key, 'T>) newKV -> table.Add newKV ) empty<'Key, 'T>
    let ofList<'Key, 'T when 'Key : equality> (elements : ('Key * 'T) list) = elements |> List.fold (fun (table : TrieMap<'Key, 'T>) newKV -> table.Add newKV ) empty<'Key, 'T>
    let ofArray<'Key, 'T when 'Key : equality> (elements : ('Key * 'T) array) = elements |> Array.fold (fun (table : TrieMap<'Key, 'T>) newKV -> table.Add newKV ) empty<'Key, 'T>

    let toSeq<'Key, 'T when 'Key : equality> (table : TrieMap<'Key, 'T>) : ('Key * 'T) seq = table :> IEnumerable<'Key * 'T>
    let toList<'Key, 'T when 'Key : equality> (table : TrieMap<'Key, 'T>) : ('Key * 'T) list = table |> Seq.toList
    let toArray<'Key, 'T when 'Key : equality> (table : TrieMap<'Key, 'T>) : ('Key * 'T) array = table |> Seq.toArray


