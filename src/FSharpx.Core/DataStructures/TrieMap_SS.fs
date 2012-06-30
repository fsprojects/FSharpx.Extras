// ----------------------------------------------------------------------------
// F# TrieMap implementation (TrieMap.fs)
// (c) Matthew Lamari, 2012, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

namespace FSharpx.DataStructures.TrieMap_SS

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

module TrieMapConstants =
    let getHashCode x = x.GetHashCode()
    let bucketCountShift = 4 // this seems the sweet spot for performance, 16 (2 ^ 4) buckets
    let shiftAtWhichYouHitBottom = (32 / bucketCountShift) * bucketCountShift
    let bucketCount = 1 <<< bucketCountShift
    let omBucketCount = bucketCount - 1
    let bucketMask = (1 <<< bucketCountShift) - 1


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
                if bucketShift = TrieMapConstants.shiftAtWhichYouHitBottom then
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
                    // make a new array - the "node" is just a single
                    let arr = Array.create<Object> TrieMapConstants.bucketCount null
                    let originalsIndex = (originalNode.Hash >>> bucketShift) &&& TrieMapConstants.bucketMask
                    let newItemIndex = (newNode.Hash >>> bucketShift) &&& TrieMapConstants.bucketMask
                    if originalsIndex = newItemIndex then
                        // they've collided, recurse
                        let result = run originalNode newNode (bucketShift + TrieMapConstants.bucketCountShift)
                        arr.[originalsIndex] <- result.Value
                        new Flagged<Object>(arr :> Object, result.Flag)
                    else
                        // they can coexist in the new array
                        arr.[originalsIndex] <- originalNode :> Object
                        // null that's default in "newNode" is fine
                        arr.[newItemIndex] <- newNode :> Object
                        new Flagged<Object>(arr :> Object, true)
            run originalNode newNode bucketShift

    // flag returns True if there was a net new node added, false if (overwrite) keeps count constant
    static member inline private augmentedArray(originalArray : Object array) (newNode : HKVNode<'Key, 'T>) bucketShift : Flagged<Object array> =
        let rec run (originalArray : Object array) newNode bucketShift = // this inner recursive, plus the inline on the outer that it allowed, gave a speed boost
            let index = (newNode.Hash >>> bucketShift) &&& TrieMapConstants.bucketMask
            let existing = originalArray.[index]
            let cloned = Array.copy originalArray

            // in attempted order of likelihood
            match existing with
            | null -> cloned.[index] <- newNode :> Object; new Flagged<Object array>(cloned, true)
            | :? (Object array) as subArray ->
                let result = run subArray newNode (bucketShift + TrieMapConstants.bucketCountShift)
                cloned.[index] <- result.Value :> Object
                new Flagged<Object array>(cloned, result.Flag)
            | :? HKVNode<'Key, 'T> as node ->
                let result = TrieMap<'Key, 'T>.settleCollision node newNode (bucketShift + TrieMapConstants.bucketCountShift)
                cloned.[index] <- result.Value
                new Flagged<Object array>(cloned, result.Flag)
            | _ -> failwith "unknown type"
        run originalArray newNode bucketShift

    member inline private this.withAddition(k : 'Key, v : 'T) : TrieMap<'Key, 'T> =
        let h = TrieMapConstants.getHashCode k
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
                let index = (keyHash >>> bucketShift) &&& TrieMapConstants.bucketMask
                match arr.[index] with
                | null -> None
                | item -> find k keyHash (bucketShift + TrieMapConstants.bucketCountShift) item
            | _ -> failwith "Unknown type on find"
        match this.rootNode with
        | null -> None
        | item -> find k (TrieMapConstants.getHashCode k) 0 item

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

    static member inline private getOriginalArrayElementCount (originalArray : Object array) : int =
        let mutable originalArrayElementCount = 0
        for i = 0 to TrieMapConstants.omBucketCount do
            if originalArray.[i] <> null then
                originalArrayElementCount <- originalArrayElementCount + 1
        originalArrayElementCount

    static member inline private getArrayDeleteResultWithIndexRemoved(originalArray : Object array) (index : int) =
        let originalArrayElementCount = TrieMap<'Key, 'T>.getOriginalArrayElementCount originalArray
        if (originalArrayElementCount = 1) then
            new Flagged<Object>(null, true) // deletion produced nothing, and it was the only thing in the array.
        else
            if originalArrayElementCount = 2 then // for the case when there's only one other thing; but it's a Node not an array
                let mutable (somethingInArrayOtherThanDeleted : Object) = null
                let mutable indexInArrayOtherThanDeleted = -1
                for i = 0 to TrieMapConstants.omBucketCount do
                    let element = originalArray.[i]
                    if (element <> null) && (i <> index) then
                        indexInArrayOtherThanDeleted <- i
                        somethingInArrayOtherThanDeleted <- element

                match somethingInArrayOtherThanDeleted with
                | :? (HKVNode<'Key, 'T>) as node when System.Object.ReferenceEquals(node.Next, null) ->
                        new Flagged<Object>(node, true) // return the node instead of the array, proliferate a loner back up the chain
                | _ -> // must be a sub-array, or multiple nodes at bottom level, either being there for a reason for a reason - keep this an array.
                    let arr = Array.create<Object> TrieMapConstants.bucketCount null
                    arr.[indexInArrayOtherThanDeleted] <- somethingInArrayOtherThanDeleted
                    new Flagged<Object>(arr, true)
            else // > 1 other item in array, so we copy the array up MINUS index = index
                let arr = Array.copy originalArray
                arr.[index] <- null
                new Flagged<Object>(arr, true)

    // returns true only if something was actually deleted
    static member private deletedFromArray(originalArray : Object array) (k : 'Key) (h : int) bucketShift : Flagged<Object>  =
        let index = (h >>> bucketShift) &&& TrieMapConstants.bucketMask
        let existing = originalArray.[index]

        match existing with
        | null -> new Flagged<Object>(originalArray, false)
        | :? (Object array) as subArray ->
            let result = TrieMap<'Key, 'T>.deletedFromArray subArray k h (bucketShift + TrieMapConstants.bucketCountShift)
            if result.Flag then
                // there was a deletion.  Several cases.
                // If it's sending up a Node, it's a lone node
                // If it's null, we have to check if this results in this array being empty.
                match result.Value with
                | null -> // see if there's only one
                    TrieMap<'Key, 'T>.getArrayDeleteResultWithIndexRemoved originalArray index
                | :? (Object array) as newSubArray -> // pretty simple, sub this in, return as a delete
                    let arr = Array.copy originalArray
                    arr.[index] <- newSubArray :> Object
                    new Flagged<Object>(arr, true)
                | :? (HKVNode<'Key, 'T>) as node -> // array delete returned a kvnode - only reason could be is if a lower list went to length of 1 and we're floating it up.
                    if (TrieMap<'Key, 'T>.getOriginalArrayElementCount originalArray) = 1 then
                        new Flagged<Object>(node, true)
                    else
                        let arr = Array.copy originalArray
                        arr.[index] <- node :> Object
                        new Flagged<Object>(arr, true)
                | _ -> failwith "unknown returned type"
            else
                new Flagged<Object>(originalArray, false)
        | :? HKVNode<'Key, 'T> as node ->
            let result = TrieMap<'Key, 'T>.deleteFromNodeList node k
            if result.Flag then // something was deleted
                match result.Value with
                | null -> TrieMap<'Key, 'T>.getArrayDeleteResultWithIndexRemoved originalArray index
                | :? (HKVNode<'Key, 'T>) as node ->
                    if ((TrieMap<'Key, 'T>.getOriginalArrayElementCount originalArray) = 1) && (System.Object.ReferenceEquals(node.Next, null)) then // solo solo - return it
                        new Flagged<Object>(node, true)
                    else
                        let arr = Array.copy originalArray
                        arr.[index] <- node :> Object
                        new Flagged<Object>(arr, true)
                | _ -> failwith "Delete of a node shouldn't return anything but null or a node"
            else
                new Flagged<Object>(originalArray, false)
        | _ -> failwith "Unknown type on delete from array"


    member inline private this.withRemoval(key : 'Key)  =
        match this.rootNode with
        | null -> TrieMap(0, Unchecked.defaultof<HKVNode<'Key, 'T>>) // already empty
        | :? (Object array) as arr ->
            let result = TrieMap<'Key, 'T>.deletedFromArray arr key (TrieMapConstants.getHashCode key) 0
            if result.Flag then TrieMap(this.count - 1, result.Value) else this
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
                arr |> Seq.map getItems |> Seq.concat
            | _ -> failwith "unknown type"
        getItems this.rootNode



    // Public Map-style accessors
    member public this.Add((key : 'Key, value : 'T) as keyValue) : TrieMap<'Key, 'T> = this.withAddition keyValue
    member public this.ContainsKey(key : 'Key) : bool = match this.findInTHash(key) with | None -> false | Some _ -> true
    member public this.Count = this.count
    member public this.IsEmpty : bool = this.count = 0
    member public this.Item(key : 'Key) : 'T = match this.findInTHash(key) with | None -> raise (KeyNotFoundException(key.ToString())) | Some value -> value
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



