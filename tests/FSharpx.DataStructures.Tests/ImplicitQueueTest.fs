module FSharpx.DataStructures.Tests.ImplicitQueueTest

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.ImplicitQueue
open NUnit.Framework
open FsUnit

[<Test>]
let ``empty should be Shallow Zero``() =
    empty |> should equal (Shallow Zero)