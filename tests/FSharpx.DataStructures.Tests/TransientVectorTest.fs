module FSharpx.DataStructures.Tests.TransientVectorTest

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.TransientVector
open NUnit.Framework
open FsUnit

[<Test>]
let ``empty vector should be empty``() =
    let x = empty<int>
    x |> count |> should equal 0

[<Test>]
let ``multiple conj to an empty vector should increase the count``() =
    empty |> conj 1 |> conj 4 |> conj 25 |> count |> should equal 3

[<Test>]
let ``conj to an empty vector should create a singleton vector``() =
    empty |> conj 1 |> nth 0 |> should equal 1

[<Test>]
let ``multiple conj to an empty vector should create a vector``() =
    empty |> conj 1 |> conj 4 |> conj 25 |> nth 1 |> should equal 4

[<Test>]
let ``300 conj to an empty vector should create a vector``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    !vector |> nth 100 |> should equal 101
    !vector |> nth 200 |> should equal 201

[<Test>]
let ``assoc an element to a nonempty transient vector should also the original vector``() =
    let v = empty |> conj "1" |> conj "4" |> conj "25" 

    v |> assocN 2 "5" |> nth 2 |> should equal "5"
    v |> nth 2 |> should equal "5"

[<Test>]
let ``vector should should be convertable to a seq``() =
    empty |> conj 1 |> conj 4 |> conj 25 |> Seq.toList |> should equal [1;4;25]

[<Test>]
let ``vector with 30000 elements should be convertable to a seq``() =
    let vector = ref empty
    for i in 1..30000 do
        vector := conj i (!vector)

    !vector |> Seq.toList |> should equal [1..30000]

[<Test>]
let ``vector with 30000 elements should allow assocN``() =
    let vector = ref empty
    for i in 1..30000 do
        vector := conj i (!vector)

    for i in 1..30000 do
        vector := assocN (i-1) (i*2) (!vector)

    !vector |> Seq.toList |> should equal [for i in 1..30000 -> i*2]

[<Test>]
let ``can peek elements from a vector``() =
    let vector = empty |> conj 1 |> conj 4 |> conj 25 
    vector |> peek |> should equal 25

[<Test>]
let ``can pop elements from a vector``() =
    let vector = empty |> conj 1 |> conj 4 |> conj 25 
    vector |> count |> should equal 3
    vector |> pop  |> count |> should equal 2
    vector |> peek |> should equal 4
    vector |> pop |> count |> should equal 1
    vector |> peek |> should equal 1

[<Test>]
let ``vector with 30000 elements should allow pop``() =
    let vector = ref empty
    for i in 1..30000 do
        vector := conj i (!vector)

    for i in 1..30000 do
        vector := pop (!vector)

    !vector |> Seq.toList |> should equal []