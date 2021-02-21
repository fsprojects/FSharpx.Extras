module FSharpx.Tests.IterateeTest

open System
open FSharpx.Functional
open FSharpx.Functional.Iteratee
open NUnit.Framework
open FsUnitTyped

[<Test>]
let ``test opt should convert a Done iteratee containing 1 into an iteratee containing Some 1``() =
  opt (returnI 1) |> run |> shouldEqual (Some 1)

[<Test>]
let ``test opt should convert a Continue iteratee containing 1 into an iteratee containing Some 1``() =
  opt (continueI <| fun s -> returnI 1) |> run |> shouldEqual (Some 1)

[<Test>]
let ``test opt should convert an Error iteratee into an iteratee containing None``() =
  opt (Error <| exn "Optional should return None") |> run |> shouldEqual None
