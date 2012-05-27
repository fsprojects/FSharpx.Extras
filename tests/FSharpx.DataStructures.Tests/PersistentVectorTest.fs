module FSharpx.DataStructures.Tests.PersistentVectorTest

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.PersistentVector
open NUnit.Framework
open FsUnit

[<Test>]
let ``empty vector should be empty``() =
    let x = empty<int>
    x.Count |> should equal 0

[<Test>]
let ``cons to an empty vector should create a singleton vector``() =
    empty |> cons 1 |> nth 0 |> should equal 1

[<Test>]
let ``multiple cons to an empty vector should create a vector``() =
    empty |> cons 1 |> cons 4 |> cons 25 |> nth 1 |> should equal 4

[<Test>]
let ``more than 200 cons to an empty vector should create a vector``() =
    let xs = [1..256]
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