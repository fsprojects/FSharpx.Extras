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