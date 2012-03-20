module FSharp.Monad.Operators.Tests.OperatorsTest

open FSharpx.Prelude
open NUnit.Framework
open FsUnit

let inline sequence (ms:'m1 list) : 'm2 =
  let mcons (p:'m1) (q:'m2) =
    (>>=) p <| fun x ->
      (>>=) q <| fun y ->
        return' (x :: y)
  List.foldBack mcons ms (return' [])

let inline mapM f xs =
  sequence (List.map f xs)

type OptionBuilder() =
  member this.Return(x) = Some x
  member this.Bind(m, f) = Option.bind f m
let maybe = OptionBuilder()

let testCases = [|
  [| box [2;4;6]; box (Some [1; 2; 3]) |]
//  [| box [1;2;3]; box None |]
|]
[<Test>]
[<TestCaseSource("testCases")>]
let ``test generic operators correctly map a list to maybe results``(input, expected) =
  let f = mapM <| fun x -> if x % 2 = 0 then Some (x/2) else None
  f input |> should equal expected
