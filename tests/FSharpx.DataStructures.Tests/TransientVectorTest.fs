module FSharpx.DataStructures.Tests.TransientVectorTest

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.Vector
open NUnit.Framework
open FsUnit

let empty() = TransientVector<int>() :> IVector<int>

[<Test>]
let ``empty vector should be empty``() =    
    empty().Count() |> should equal 0

[<Test>]
let ``multiple conj to an empty vector should increase the count``() =
    empty().Conj(1).Conj(4).Conj(25).Count() |> should equal 3

[<Test>]
let ``conj to an empty vector should create a singleton vector``() =
    empty().Conj(1).[0] |> should equal 1

[<Test>]
let ``multiple conj to an empty vector should create a vector``() =
    empty().Conj(1).Conj(4).Conj(25).[1] |> should equal 4

[<Test>]
let ``300 conj to an empty vector should create a vector``() =
    let vector = ref (empty())
    for i in 1..300 do
        vector := (!vector).Conj i

    (!vector).[100] |> should equal 101
    (!vector).[200] |> should equal 201

[<Test>]
let ``assoc an element to a nonempty transient vector should also the original vector``() =
    let v = (TransientVector<string>() :> IVector<string>).Conj("1").Conj("4").Conj("25")

    v.AssocN(2,"5").[2] |> should equal "5"
    v.[2] |> should equal "5"

[<Test>]
let ``vector should should be convertable to a seq``() =
    empty().Conj(1).Conj(4).Conj(25) |> Seq.toList |> should equal [1;4;25]

[<Test>]
let ``vector with 30000 elements should be convertable to a seq``() =
    let vector = ref (empty())
    for i in 1..30000 do
        vector := (!vector).Conj i

    !vector |> Seq.toList |> should equal [1..30000]

[<Test>]
let ``vector with 30000 elements should allow assocN``() =
    let vector = ref (empty())
    for i in 1..30000 do
        vector := (!vector).Conj i

    for i in 1..30000 do
        vector := (!vector).AssocN(i-1,i*2)

    !vector |> Seq.toList |> should equal [for i in 1..30000 -> i*2]

[<Test>]
let ``can peek elements from a vector``() =
    let vector = empty().Conj(1).Conj(4).Conj(25)
    vector.Peek() |> should equal 25

[<Test>]
let ``can pop elements from a vector``() =
    let vector = empty().Conj(1).Conj(4).Conj(25)
    vector.Count() |> should equal 3
    vector.Pop().Count()  |> should equal 2
    vector.Peek() |> should equal 4
    vector.Pop().Count()  |> should equal 1
    vector.Peek() |> should equal 1

[<Test>]
let ``vector with 30000 elements should allow pop``() =
    let vector = ref (empty())
    for i in 1..30000 do
        vector := (!vector).Conj i

    for i in 1..30000 do
        vector := (!vector).Pop()

    !vector |> Seq.toList |> should equal []