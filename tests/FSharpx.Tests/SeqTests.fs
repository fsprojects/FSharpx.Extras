module FSharpx.Tests.SeqTests

open FSharpx
open NUnit.Framework
open FsUnit

[<Test>]
let index() =
  let a = {'a'..'z'}
  Seq.index a |> Seq.take 5 |> Seq.toList
  |> should equal [0,'a'; 1,'b'; 2,'c'; 3,'d'; 4,'e']

[<Test>]
let tryFindWithIndex_None() =
  let a = {1..10}
  Seq.tryFindWithIndex ((<)10) a
  |> should equal None

[<Test>]
let tryFindWithIndex_Some() =
  let a = {'a'..'z'}
  Seq.tryFindWithIndex ((=)'e') a
  |> should equal (Some (4,'e'))

[<Test>]
let lift2() =
    Seq.lift2 (+) [0;1] [0;2] |> Seq.toList
    |> should equal [0;2;1;3]