module FSharpx.Tests.HeapTests

open System
open FsUnit
open FSharpx
open FSharpx.Collections
open NUnit.Framework

[<Test>]
let ``Stack does not overflow when calling tail on a large heap`` () = 
    let rnd = new Random()
    let h = 
        [1..1000000] 
        |> Seq.sortBy (fun x -> rnd.Next())
        |> Heap.ofSeq false
    
    Heap.tail h |> ignore

open FsCheck
open FsCheck.NUnit

[<Test>]
let ``compare to list``() =
    fsCheck "head" <| 
        fun (xs : list<int>) ->
            xs |> List.isEmpty |> not ==>
                (fun () ->
                    let listHead = xs |> List.sort |> List.head
                    let heapHead = xs |> Heap.ofSeq false |> Heap.head
                    listHead = heapHead)

    fsCheck "tryHead" <| 
        fun (xs : list<int>) ->
            let listHead = xs |> List.sort |> Seq.tryHead
            let heapHead = xs |> Heap.ofSeq false |> Heap.tryHead
            listHead = heapHead

    fsCheck "tail" <| 
        fun (xs : list<int>) ->
            xs |> List.isEmpty |> not ==>
                (fun () ->
                    let listTail = xs |> List.sort |> List.tail
                    let heapTail = xs |> Heap.ofSeq false |> Heap.tail
                    listTail = (heapTail |> List.ofSeq))

    fsCheck "tryTail" <| 
        fun (xs : list<int>) ->
            let heapTail = xs |> Heap.ofSeq false |> Heap.tryTail
            match xs |> List.length with
            | 0 ->
                heapTail = None
            | _ ->
                let listTail = xs |> List.sort |> List.tail
                Some listTail = (heapTail |> Option.map List.ofSeq)

    fsCheck "insert" <| 
        fun (xs : list<int>) ->
            let listSorted = xs |> List.sort
            let heap = xs |> List.fold (flip Heap.insert) (Heap.empty false)
            listSorted = (heap |> List.ofSeq)

    fsCheck "length" <| 
        fun (xs : list<int>) ->
            let heap = xs |> Heap.ofSeq false
            (xs |> List.length) = (heap |> Heap.length)