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
        val private rootNode : Entry<'Key, 'T>
        private new(count : int, rootNode : Entry<'Key, 'T>) = { count = count; rootNode = rootNode }
    end

    // flag returns True if there was a net new node added, false if (overwrite) keeps count constant
    // can return a new node OR an array
    static member (*inline*) private settleCollision(originalNode : HKVNode<'Key, 'T>) (newNode : HKVNode<'Key, 'T>) bucketShift : Flagged<Entry<'Key, 'T>> =
        if newNode.Key = originalNode.Key then // this takes care of the singleton on singleton AND the head of the list
            newNode.Next <- originalNode.Next
            Flagged<Entry<'Key, 'T>>(HKVNode newNode, false)
        else
            let rec run originalNode newNode bucketShift = // bit of a speed boost here, can (*inline*) outer function, and not repeat the first (check for equal) clause.
                if bucketShift = TrieMapConstants.shiftAtWhichYouHitBottom then
                    // hit bottom - sort it out, find if already exists and rebuild accordingly

                    // going for speed here
                    let mutable current = originalNode.Next // remember - we've checked for the match of hkv being at the TOP at the beginning of the outer function
                    let mutable amDone = current.IsNone
                    let mutable wasFound = false
                    let mutable (afterFound : HKVNode<'Key, 'T> option) = None // whatever - can't nicely assign it to null, we depend on the flag WasFound
                    while not amDone do
                        let cv = current.Value
                        if cv.Key = newNode.Key then
                            // found own key
                            wasFound <- true
                            afterFound <- cv.Next
                            amDone <- true
                        else
                            if cv.Next.IsNone then
                                amDone <- true
                            else current <- cv.Next

                    match wasFound with
                    | false->
                        newNode.Next <- Some originalNode
                        Flagged<Entry<'Key, 'T>>(HKVNode newNode, true)
                    | true -> // found ourselves, have to overwrite.  Trust afterFound now.
                        newNode.Next <- afterFound
                        let mutable rl = newNode
                        let mutable current = originalNode
                        while (not (current.Key = newNode.Key)) do
                            rl <- { current with Next = Some rl }
                            current <- current.Next.Value // assumed value as we found our key in there, so we won't hit end
                        Flagged<Entry<'Key, 'T>>(HKVNode rl, false) // no net add
                else
                    // make a new array - the "node" is just a single
                    let arr = Array.create<Entry<'Key, 'T>> TrieMapConstants.bucketCount Null
                    let originalsIndex = (originalNode.Hash >>> bucketShift) &&& TrieMapConstants.bucketMask
                    let newItemIndex = (newNode.Hash >>> bucketShift) &&& TrieMapConstants.bucketMask
                    if originalsIndex = newItemIndex then
                        // they've collided, recurse
                        let result = run originalNode newNode (bucketShift + TrieMapConstants.bucketCountShift)
                        arr.[originalsIndex] <- result.Value
                        Flagged<Entry<'Key, 'T>>(Buckets arr, result.Flag)
                    else
                        // they can coexist in the new array
                        arr.[originalsIndex] <- HKVNode originalNode
                        // null that's default in "newNode" is fine
                        arr.[newItemIndex] <- HKVNode newNode
                        Flagged<Entry<'Key, 'T>>(Buckets arr, true)
            run originalNode newNode bucketShift

    // flag returns True if there was a net new node added, false if (overwrite) keeps count constant
    static member (*inline*) private augmentedArray(originalArray : Entry<'Key, 'T> array) (newNode : HKVNode<'Key, 'T>) bucketShift : Flagged<Entry<'Key, 'T> array> =
        let rec run (originalArray : Entry<'Key, 'T> array) newNode bucketShift = // this inner recursive, plus the (*inline*) on the outer that it allowed, gave a speed boost
            let index = (newNode.Hash >>> bucketShift) &&& TrieMapConstants.bucketMask
            let existing = originalArray.[index]
            let cloned = Array.copy originalArray

            // in attempted order of likelihood
            match existing with
            | Null -> cloned.[index] <- HKVNode newNode; Flagged<Entry<'Key, 'T> array>(cloned, true)
            | Buckets subArray ->
                let result = run subArray newNode (bucketShift + TrieMapConstants.bucketCountShift)
                cloned.[index] <- Buckets result.Value
                Flagged<Entry<'Key, 'T> array>(cloned, result.Flag)
            | HKVNode node ->
                let result = TrieMap<'Key, 'T>.settleCollision node newNode (bucketShift + TrieMapConstants.bucketCountShift)
                cloned.[index] <- result.Value
                Flagged<Entry<'Key, 'T> array>(cloned, result.Flag)
            | _ -> failwith "unknown type"
        run originalArray newNode bucketShift

    member (*inline*) private this.withAddition(k : 'Key, v : 'T) : TrieMap<'Key, 'T> =
        let h = TrieMapConstants.getHashCode k
        //let hkv = new HKV<'Key, 'T>(k, v, h)
        let newNode = { Key = k; Value = v; Hash = h; Next = None }
        match this.rootNode with
        | Buckets arr ->
            let result = TrieMap.augmentedArray arr newNode 0
            TrieMap((if result.Flag then this.count + 1 else this.count), Buckets result.Value)
        | Null -> // first node
            new TrieMap<'Key, 'T>(1, HKVNode newNode)
        | HKVNode node ->
            let result = TrieMap.settleCollision node newNode 0
            TrieMap((if result.Flag then this.count + 1 else this.count), result.Value)
        | _ -> failwith "unknown type"

    member (*inline*) private this.findInTHash (k : 'Key) : 'T option =
        let rec find k (keyHash : int) (bucketShift : int) node = // deliberately don't let null in
            match node with
            | HKVNode node ->
                let mutable found = false
                let mutable foundAt = node.Next
                if node.Key = k then
                    Some node.Value
                else
                    let mutable current = node.Next
                    while (not found) && (current.IsSome) do
                        let cv = current.Value
                        if cv.Key = k then
                            found <- true
                            foundAt <- current
                        else
                            current <- cv.Next
                    if found then Some current.Value.Value else None
            | Buckets arr ->
                let index = (keyHash >>> bucketShift) &&& TrieMapConstants.bucketMask
                find k keyHash (bucketShift + TrieMapConstants.bucketCountShift) arr.[index]
            | Null -> None
        find k (TrieMapConstants.getHashCode k) 0 this.rootNode

    static member (*inline*) private deleteFromNodeList (nodeList : HKVNode<'Key, 'T>) (k : 'Key) : Flagged<HKVNode<'Key, 'T> option> =
        let mutable found = false
        let mutable (afterFound : HKVNode<'Key, 'T> option) = None // whatever - can't nicely assign it to null, we depend on the flag WasFound
        let mutable current = nodeList
        let mutable amDone = false
        while not amDone do
            let cv = current
            if cv.Key = k then
                afterFound <- cv.Next
                amDone <- true
                found <- true
            else
                if cv.Next.IsNone then
                    amDone <- true
                else
                    current <- cv.Next.Value
        if found then
            let mutable rl = afterFound
            let mutable current = nodeList
            while (not (current.Key = k)) do
                rl <- Some { current with Next = rl }
                current <- current.Next.Value // assumed value as we found our key in there, so we won't hit end
            Flagged<HKVNode<'Key, 'T> option>(rl, true)
        else
            Flagged<HKVNode<'Key, 'T> option>(Some nodeList, false) // not found

    static member (*inline*) private getOriginalArrayElementCount (originalArray : Entry<'Key, 'T> array) : int =
        let mutable originalArrayElementCount = 0
        for i = 0 to TrieMapConstants.omBucketCount do
            match originalArray.[i] with
            | Null -> ()
            | _ -> originalArrayElementCount <- originalArrayElementCount + 1
        originalArrayElementCount

    static member (*inline*) private getArrayDeleteResultWithIndexRemoved(originalArray : Entry<'Key, 'T> array) (index : int) : Flagged<Entry<'Key, 'T>> =
        let originalArrayElementCount = TrieMap<'Key, 'T>.getOriginalArrayElementCount originalArray
        if (originalArrayElementCount = 1) then
            Flagged<Entry<'Key, 'T>>(Null, true) // deletion produced nothing, and it was the only thing in the array.
        else
            if originalArrayElementCount = 2 then // for the case when there's only one other thing; but it's a Node not an array
                let mutable somethingInArrayOtherThanDeleted = Null
                let mutable indexInArrayOtherThanDeleted = -1
                for i = 0 to TrieMapConstants.omBucketCount do
                    let element = originalArray.[i]
                    match element with
                    | Null -> ()
                    | _ when i <> index -> indexInArrayOtherThanDeleted <- i; somethingInArrayOtherThanDeleted <- element
                    | _ -> ()

                match somethingInArrayOtherThanDeleted with
                | HKVNode node when node.Next.IsNone ->
                        Flagged<Entry<'Key, 'T>>(somethingInArrayOtherThanDeleted, true) // return the node instead of the array, proliferate a loner back up the chain
                | _ -> // must be a sub-array, or multiple nodes at bottom level, either being there for a reason for a reason - keep this an array.
                    let arr = Array.create<Entry<'Key, 'T>> TrieMapConstants.bucketCount Null
                    arr.[indexInArrayOtherThanDeleted] <- somethingInArrayOtherThanDeleted
                    Flagged<Entry<'Key, 'T>>(Buckets arr, true)
            else // > 1 other item in array, so we copy the array up MINUS index = index
                let arr = Array.copy originalArray
                arr.[index] <- Null
                Flagged<Entry<'Key, 'T>>(Buckets arr, true)

    // returns true only if something was actually deleted
    static member private deletedFromArray(originalArray : Entry<'Key, 'T> array) (k : 'Key) (h : int) bucketShift : Flagged<Entry<'Key, 'T>>  =
        let index = (h >>> bucketShift) &&& TrieMapConstants.bucketMask
        let existing = originalArray.[index]

        match existing with
        | Null -> Flagged<Entry<'Key, 'T>>(Buckets originalArray, false)
        | Buckets subArray ->
            let result = TrieMap<'Key, 'T>.deletedFromArray subArray k h (bucketShift + TrieMapConstants.bucketCountShift)
            if result.Flag then
                // there was a deletion.  Several cases.
                // If it's sending up a Node, it's a lone node
                // If it's null, we have to check if this results in this array being empty.
                match result.Value with
                | Null -> // see if there's only one
                    TrieMap<'Key, 'T>.getArrayDeleteResultWithIndexRemoved originalArray index
                | Buckets newSubArray -> // pretty simple, sub this in, return as a delete
                    let arr = Array.copy originalArray
                    arr.[index] <- Buckets newSubArray
                    Flagged<Entry<'Key, 'T>>(Buckets arr, true)
                | HKVNode node -> // array delete returned a kvnode - only reason could be is if a lower list went to length of 1 and we're floating it up.
                    if (TrieMap<'Key, 'T>.getOriginalArrayElementCount originalArray) = 1 then
                        Flagged<Entry<'Key, 'T>>(result.Value, true)
                    else
                        let arr = Array.copy originalArray
                        arr.[index] <- HKVNode node
                        Flagged<Entry<'Key, 'T>>(Buckets arr, true)
                | _ -> failwith "unknown returned type"
            else
                Flagged<Entry<'Key, 'T>>(Buckets originalArray, false)
        | HKVNode node ->
            let result = TrieMap<'Key, 'T>.deleteFromNodeList node k
            if result.Flag then // something was deleted
                match result.Value with
                | None -> TrieMap<'Key, 'T>.getArrayDeleteResultWithIndexRemoved originalArray index // a loner was wiped
                | Some node ->
                    if ((TrieMap<'Key, 'T>.getOriginalArrayElementCount originalArray) = 1) && node.Next.IsNone then // solo solo - return it
                        Flagged<Entry<'Key, 'T>>(HKVNode node, true)
                    else
                        let arr = Array.copy originalArray
                        arr.[index] <- HKVNode node
                        Flagged<Entry<'Key, 'T>>(Buckets arr, true)
            else
                Flagged<Entry<'Key, 'T>>(Buckets originalArray, false)


    member (*inline*) private this.withRemoval(key : 'Key)  =
        match this.rootNode with
        | Null -> this // already empty
        | Buckets arr ->
            let result = TrieMap<'Key, 'T>.deletedFromArray arr key (TrieMapConstants.getHashCode key) 0
            if result.Flag then TrieMap(this.count - 1, result.Value) else this
        | HKVNode node ->
            assert node.Next.IsNone
            if node.Key = key then TrieMap(0, Null) else this

    member (*inline*) private this.getTHashKVPairs () : ('Key * 'T) seq =
        let rec getItems (node : Entry<'Key, 'T>) : ('Key * 'T) seq =
            match node with
            | Null -> Seq.empty
            | HKVNode node ->
                let rec run current =
                    seq { yield (current.Key, current.Value); if current.Next.IsSome then run current.Next.Value }
                run node
            | Buckets arr ->
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

    static member Empty = new TrieMap<'Key, 'T>(0, Null)

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
    mutable Next : HKVNode<'Key, 'T> option // this lets HKVNode perform double-duty, being mutated when put in place after being passed down a chain.  Never mutated once placed
    }
and
 private Entry<'Key, 'T when 'Key : equality> =
    | Buckets of Entry<'Key, 'T> array
    | HKVNode of HKVNode<'Key, 'T>
    | Null





module TrieMap =
    // these attempt to mimic the behavior of the Map module equivalents

    let add key value (table : TrieMap<'Key, 'T>) = table.Add(key, value)
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


    let remove key (table : TrieMap<'Key, 'T>) =
        let withRemoval = table.Remove key
        let missing = table |> toSeq |> Seq.map fst |> Seq.filter (fun x -> not (withRemoval.ContainsKey x)) |> List.ofSeq
        if missing |> Seq.filter (fun x -> x <> key) |> (Seq.isEmpty >> not) then
            table.Remove key |> ignore
            table.Remove key |> ignore
            table.Remove key |> ignore
            Console.WriteLine "Problem"
        withRemoval


