module FSharpx.DataStructures.Tests.PersistentVectorTest

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.PersistentVector
open NUnit.Framework
open FsUnit

[<Test>]
let ``empty vector should be empty``() =
    let x = empty<int>
    x |> count |> should equal 0

[<Test>]
let ``multiple cons to an empty vector should increase the count``() =
    empty |> cons 1 |> cons 4 |> cons 25 |> count |> should equal 3

[<Test>]
let ``cons to an empty vector should create a singleton vector``() =
    empty |> cons 1 |> nth 0 |> should equal 1

[<Test>]
let ``multiple cons to an empty vector should create a vector``() =
    empty |> cons 1 |> cons 4 |> cons 25 |> nth 1 |> should equal 4

[<Test>]
let ``300 cons to an empty vector should create a vector``() =
    let vector = ref empty
    for i in 1..300 do
        vector := cons i (!vector)

    !vector |> nth 100 |> should equal 101
    !vector |> nth 200 |> should equal 201

[<Test>]
let ``assoc an element to a nonempty vector should not change the original vector``() =
    let v = empty |> cons "1" |> cons "4" |> cons "25" 

    v |> assocN 2 "5" |> nth 2 |> should equal "5"
    v |> nth 2 |> should equal "25"

[<Test>]
let ``vector should should be convertable to a seq``() =
    empty |> cons 1 |> cons 4 |> cons 25  |> Seq.toList |> should equal [1;4;25]

[<Test>]
let ``vector with 30000 elements should be convertable to a seq``() =
    let vector = ref empty
    for i in 1..30000 do
        vector := cons i (!vector)

    !vector |> Seq.toList |> should equal [1..30000]

[<Test>]
let ``vector can be created from a seq``() =
    let xs = [7;88;1;4;25;30] 
    ofSeq xs |> Seq.toList |> should equal xs

[<Test>]
let ``vector with 30000 elements should allow assocN``() =
    let vector = ref empty
    for i in 1..30000 do
        vector := cons i (!vector)

    for i in 1..30000 do
        vector := assocN (i-1) (i*2) (!vector)

    !vector |> Seq.toList |> should equal [for i in 1..30000 -> i*2]

[<Test>]
let ``can peek elements from a vector``() =
    let vector = empty |> cons 1 |> cons 4 |> cons 25 
    vector |> peek |> should equal 25
    
[<Test>]
let ``can pop elements from a vector``() =
    let vector = empty |> cons 1 |> cons 4 |> cons 25 
    vector |> peek |> should equal 25
    vector |> pop |> peek |> should equal 4
    vector |> pop |> pop |> peek |> should equal 1

    vector |> count |> should equal 3
    vector |> pop |> count |> should equal 2
    vector |> pop |> pop |> count |> should equal 1

[<Test>]
let ``vector with 30000 elements should allow pop``() =
    let vector = ref empty
    for i in 1..30000 do
        vector := cons i (!vector)

    for i in 1..30000 do
        vector := pop (!vector)

    !vector |> Seq.toList |> should equal []