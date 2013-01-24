module FSharpx.Collections.Experimental.Tests.BootstrappedQueueTest

open System
open FSharpx.Collections.Experimental
open FSharpx.Collections.Experimental.BootstrappedQueue
open NUnit.Framework
open FsUnit

[<Test>]
let ``empty queue should be empty``() =
    isEmpty empty |> should equal true

[<Test>]
let ``it should allow to enqueue``() =
    empty |> snoc 1 |> snoc 2 |> isEmpty |> should equal false

[<Test>]
let ``it should allow to dequeue``() =
    empty |> snoc 1 |> tail |> isEmpty |> should equal true

[<Test>]
let ``it should fail if there is no head in the queue``() =
    let ok = ref false
    try
        empty |> head |> ignore
    with x when x = Exceptions.Empty -> ok := true
    !ok |> should equal true

[<Test>]
let ``it should give None if there is no head in the queue``() =
    empty |> tryGetHead |> should equal None

[<Test>]
let ``it should fail if there is no tail the queue``() =
    let ok = ref false
    try
        empty |> tail |> ignore
    with x when x = Exceptions.Empty -> ok := true
    !ok |> should equal true

[<Test>]
let ``it should give None if there is no tail in the queue``() =
    empty |> tryGetTail |> should equal None

[<Test>]
let ``it should allow to get the head from a queue``() =
    empty |> snoc 1 |> snoc 2 |> head |> should equal 1

[<Test>]
let ``it should allow to get the head from a queue safely``() =
    empty |> snoc 1 |> snoc 2 |> tryGetHead |> should equal (Some 1)

[<Test>]
let ``it should allow to get the tail from the queue``() =
    empty |> snoc "a" |> snoc "b" |> snoc "c" |> tail |> head |> should equal "b"

[<Test>]
let ``it should allow to get the tail from a queue safely``() =
    let value = empty |> snoc 1 |> snoc 2 |> tryGetTail
    value.Value |> head |> should equal 2

[<Test>]
let ``it should initialize from a list``() =
    ofList [1..10] |> snoc 11 |> snoc 12 |> tail |> length |> should equal 11