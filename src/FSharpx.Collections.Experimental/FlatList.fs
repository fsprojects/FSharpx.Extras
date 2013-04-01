//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------
(*
Notification in accordance with Apache License, Version 2.0, section 4, changes for FSharpx implementation:
1) namespace changes
2) source lines not required eliminated
3) module let binding "one" renamed to "singleton"
4) module let bindings re-ordered (alphabetized)
5) file signature now controlled by signature file
6) added "rev" to module
*)


namespace FSharpx.Collections.Experimental 

open System.Collections
open System.Collections.Generic

[<Struct>]
type FlatList<'T> =
    val internal array : 'T[]
    internal new (arr: 'T[]) = { array = (match arr with null -> null | arr -> if arr.Length = 0 then null else arr) }
    member x.Item with get(n:int) = x.array.[n]
    member x.Length = match x.array with null -> 0 | arr -> arr.Length
    member x.IsEmpty = match x.array with null -> true | _ -> false
    static member Empty : FlatList<'T> = FlatList(null)
    interface IEnumerable<'T> with 
        member x.GetEnumerator() : IEnumerator<'T> = 
            match x.array with 
            | null -> Seq.empty.GetEnumerator()
            | arr -> (arr :> IEnumerable<'T>).GetEnumerator()
    interface IEnumerable with 
        member x.GetEnumerator() : IEnumerator = 
            match x.array with 
            | null -> (Seq.empty :> IEnumerable).GetEnumerator()
            | arr -> (arr :> IEnumerable).GetEnumerator()


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FlatList =

    let append(l1 : FlatList<'T>) (l2 : FlatList<'T>) = 
        match l1.array, l2.array with 
        | null,_ -> l2
        | _,null -> l1
        | arr1, arr2 -> FlatList(Array.append arr1 arr2)

    let collect (f: 'T -> FlatList<'T>) (x:FlatList<_>) = 
        match x.array with 
        | null -> FlatList.Empty 
        | arr -> 
           if arr.Length = 1 then f arr.[0]
           else FlatList(Array.map (fun x -> match (f x).array with null -> [| |] | arr -> arr) arr |> Array.concat)

    let concat (x:FlatList<_>) = 
        match x.array with 
        | null -> FlatList.Empty 
        | arr -> FlatList(Array.concat arr)

    let empty<'T> = FlatList<'T>.Empty

    let exists f (x:FlatList<_>) = 
        match x.array with 
        | null -> false 
        | arr -> Array.exists f arr

    let filter f (x:FlatList<_>) = 
        match x.array with 
        | null -> FlatList.Empty 
        | arr -> FlatList(Array.filter f arr)

    let fold f acc (x:FlatList<_>) = 
        match x.array with 
        | null -> acc 
        | arr -> Array.fold f acc arr

    let fold2 f acc (x:FlatList<_>) (y:FlatList<_>) = 
        match x.array,y.array with 
        | null,null -> acc 
        | null,_ | _,null -> invalidArg "x" "mismatched list lengths"
        | arr1,arr2 -> Array.fold2 f acc arr1 arr2

    let foldBack f (x:FlatList<_>) acc  = 
        match x.array with 
        | null -> acc 
        | arr -> Array.foldBack f arr acc

    let foldBack2 f (x:FlatList<_>) (y:FlatList<_>) acc = 
        match x.array,y.array with 
        | null,null -> acc 
        | null,_ | _,null -> invalidArg "x" "mismatched list lengths"
        | arr1,arr2 -> Array.foldBack2 f arr1 arr2 acc

    let forall f (x:FlatList<_>) = 
        match x.array with 
        | null -> true 
        | arr -> Array.forall f arr

    let forall2 f (x1:FlatList<_>) (x2:FlatList<_>) = 
        match x1.array, x2.array with 
        | null,null -> true
        | null,_ | _,null -> invalidArg "x1" "mismatched list lengths"
        | arr1,arr2 -> Array.forall2 f arr1 arr2

    let init n f = 
        if n = 0 then 
            FlatList.Empty 
        else 
            FlatList(Array.init n f)

    let isEmpty (x:FlatList<_>) = x.IsEmpty

    let iter f (x:FlatList<_>) = 
        match x.array with 
        | null -> ()
        | arr -> Array.iter f arr

    let iter2 f (x1:FlatList<_>) (x2:FlatList<_>) = 
        match x1.array, x2.array with 
        | null,null -> ()
        | null,_ | _,null -> invalidArg "x1" "mismatched list lengths"
        | arr1,arr2 -> Array.iter2 f arr1 arr2

    let iteri f (x:FlatList<_>) = 
        match x.array with 
        | null -> ()
        | arr -> Array.iteri f arr

    let length (x:FlatList<_>) = x.Length

    let map f (x:FlatList<_>) = 
        match x.array with 
        | null -> FlatList.Empty 
        | arr -> FlatList(Array.map f arr)

    let map2 f (x:FlatList<_>) (y:FlatList<_>) = 
        match x.array,y.array with 
        | null,null -> FlatList.Empty 
        | null,_ | _,null -> invalidArg "x" "mismatched list lengths"
        | arr1,arr2 -> FlatList(Array.map2 f arr1 arr2)

    let mapi f (x:FlatList<_>) = 
        match x.array with 
        | null -> FlatList.Empty 
        | arr -> FlatList(Array.mapi f arr)

    let ofList l = 
        match l with 
        | [] -> FlatList.Empty 
        | l -> FlatList(Array.ofList l)

    let ofSeq l = 
        FlatList(Array.ofSeq l)

    let partition f (x:FlatList<_>) = 
        match x.array with 
        | null -> FlatList.Empty,FlatList.Empty 
        | arr -> 
            let arr1,arr2 = Array.partition f arr 
            FlatList(arr1),FlatList(arr2)

    let physicalEquality (x:FlatList<_>) (y:FlatList<_>) = 
        LanguagePrimitives.PhysicalEquality x.array y.array 

    let rev (x:FlatList<_>) = 
        match x.array with 
        | null -> FlatList.Empty 
        | arr -> FlatList(Array.rev arr)

    let singleton(x) = FlatList([| x |])

    let sum (x:FlatList<int>) = 
        match x.array with 
        | null -> 0 
        | arr -> Array.sum arr

    let sumBy (f: 'T -> int) (x:FlatList<'T>) = 
        match x.array with 
        | null -> 0 
        | arr -> Array.sumBy f arr

    let toList (x:FlatList<_>) = 
        match x.array with 
        | null -> [] 
        | arr -> Array.toList arr

    let toMap (x:FlatList<_>) = match x.array with null -> Map.empty | arr -> Map.ofArray arr

    let tryFind f (x:FlatList<_>) = 
        match x.array with 
        | null -> None 
        | arr -> Array.tryFind f arr

    let unzip (x:FlatList<_>) = 
        match x.array with 
        | null -> FlatList.Empty,FlatList.Empty 
        | arr -> let arr1,arr2 = Array.unzip arr in FlatList(arr1),FlatList(arr2)

    let zip (x:FlatList<_>) (y:FlatList<_>) = 
        match x.array,y.array with 
        | null,null -> FlatList.Empty
        | null,_ | _,null -> invalidArg "x" "mismatched list lengths"
        | arr1,arr2 -> FlatList(Array.zip arr1 arr2)