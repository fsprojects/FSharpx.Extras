module FSharpx.Collections.Experimental.Tests.DListTest

open System
open FSharpx.Collections.Experimental
open FSharpx.Collections.Experimental.DList
open NUnit.Framework
open FsUnit

[<Test>]
let ``test should verify empty is Nil``() =
  empty<_> |> should equal Nil

let expected = Join(Unit 0, Join(Unit 1, Join(Unit 2, Join(Unit 3, Unit 4, 2), 3), 4), 5)
let expected2 = Join(Unit 1, Join(Unit 2, Join(Unit 3, Unit 4, 2), 3), 4)

[<Test>]
let ``test length should return 5``() =
  length expected |> should equal 5

[<Test>]
let ``test ofSeq should create a DList from a seq``() =
  let test = seq { for i in 0..4 -> i }
  DList.ofSeq test |> should equal expected

[<Test>]
let ``test ofSeq should create a DList from a list``() =
  let test = [ for i in 0..4 -> i ]
  DList.ofSeq test |> should equal expected

[<Test>]
let ``test ofSeq should create a DList from an array``() =
  let test = [| for i in 0..4 -> i |]
  DList.ofSeq test |> should equal expected

[<Test>]
let ``test cons should prepend 10 to the front of the original list``() =
  cons 10 expected |> should equal (Join(Unit 10, expected, 6))

[<Test>]
let ``test singleton should return a Unit containing the solo value``() =
  singleton 1 |> should equal (Unit 1)

[<Test>]
let ``test cons should return a Unit when the tail is Nil``() =
  cons 1 DList.empty |> should equal (Unit 1)

[<Test>]
let ``test subsequent cons should create a DList just as the constructor functions``() =
  cons 0 (cons 1 (cons 2 (cons 3 (cons 4 empty)))) |> should equal expected

[<Test>]
let ``test append should join two DLists together``() =
  append expected expected2 |> should equal (Join(expected, expected2, 9))

[<Test>]
let ``test snoc should join DList and one element together``() =
  snoc expected 5 |> should equal (Join(expected, Unit 5, 6))

[<Test>]
let ``test head should return the first item in the DList``() =
  head (append expected expected) |> should equal 0

[<Test>]
let ``test tail should return all items except the head``() =
  tail (append expected expected) |> should equal (Join(cons 1 (cons 2 (cons 3 (cons 4 empty))), expected, 9))

[<Test>]
let ``test DList should respond to Seq functions such as map``() =
  let testmap x = x*x
  let actual = Seq.map testmap (append expected expected)
  let expected = seq { yield 0; yield 1; yield 2; yield 3; yield 4; yield 0; yield 1; yield 2; yield 3; yield 4 } |> Seq.map testmap
  Assert.That(actual, Is.EquivalentTo expected)