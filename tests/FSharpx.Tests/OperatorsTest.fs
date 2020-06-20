module FSharp.Monad.Operators.Tests.OperatorsTest

open FSharpx.Operators
open FSharpx.Functional
open NUnit.Framework

let inline sequence (monad1:^M1) (monad2:^M2) (ms:'m1 list) : 'm2 =
  let mcons (p:'m1) (q:'m2) =
    bindM monad1 p <| fun x ->
      bindM monad2 q <| fun y ->
        returnM monad2 (x :: y)
  List.foldBack mcons ms (returnM monad2 [])

let inline mapM monad1 monad2 f xs =
  sequence monad1 monad2 (List.map f xs)

type OptionBuilder() =
  member this.Return(x) = Some x
  member this.Bind(m, f) = Option.bind f m
let maybe = OptionBuilder()

let testCases = [
  TestCaseData([2;4;6], ExpectedResult = Some [1; 2; 3])
  TestCaseData([1;2;3], ExpectedResult = None)
]

[<Test>]
[<TestCaseSource("testCases")>]
let ``test generic operators correctly map a list to maybe results`` input =
  let f = mapM maybe maybe <| fun x -> if x % 2 = 0 then Some (x/2) else None
  f input
