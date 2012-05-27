module FSharpx.DataStructures.Tests.BottomUpMergeSortTest

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.BottomUpMergeSort
open NUnit.Framework
open FsUnit

[<Test>]
let ``empty list should be empty``() =
    empty |> isEmpty |> should equal true

[<Test>]
let ``empty list should be empty after sort``() =
    sort empty |> should equal []

[<Test>]
let ``singleton list should be the same after sort``() =
    sort (singleton 1) |> should equal [1]

[<Test>]
let ``adding a element to an empty list``() =
    empty |> add 1 |> sort |> should equal [1]

[<Test>]
let ``adding multiple elements to an empty list``() =
    empty |> add 100 |> add 1 |> add 3 |> add 42 |> sort |> should equal [1; 3; 42; 100]

[<Test>]
let ``adding multiple strings to an empty list``() =
    empty |> add "100" |> add "1" |> add "3" |> add "42" |> sort |> should equal ["1"; "100"; "3"; "42"]