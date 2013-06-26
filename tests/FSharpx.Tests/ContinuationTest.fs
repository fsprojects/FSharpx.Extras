module FSharpx.Tests.ContinuationTest

open System
open FSharpx
open FSharpx.Continuation
open NUnit.Framework
open FsUnit

let c n = cont { return n }
let addSomeNumbers = cont {
  let! x = c 6
  let! y = c 7
  return x + y }

[<Test>]
let ``When adding 6 to 7 and applying a continuation to convert to string and replace 1 with a, it should return a3``() =
  runCont addSomeNumbers (fun x -> x.ToString().Replace('1', 'a')) (sprintf "%A") |> should equal "a3"

[<Test>]
let ``When throwing an exception, it should catch the exception``() =
  let message = "failed"
  runCont (Continuation.throw (Exception(message))) (fun _ -> "didn't handle exception") (fun (e:exn) -> e.Message) |> should equal message

(* Test callCC *)
let sum l =
  let rec sum l = cont {
    let! result = callcc (fun exit1 -> cont {
      match l with
      | [] -> return 0
      | h::t when h = 2 -> return! exit1 42
      | h::t -> let! r = sum t
                return h + r })
    return result }
  runCont (sum l) id (fun _ -> -1)

[<Test>]
let ``When summing a list without a 2 via callCC it should return 8``() =
  sum [1;1;3;3] |> should equal 8

[<Test>]
let ``When summing a list containing 2 via callCC it should return 43``() =
  sum [1;2;3] |> should equal 43

(* Test Coroutine *)
[<Test>]
let ``When running a coroutine it should yield elements in turn``() =
  // This test comes from the sample on http://fssnip.net/7M
  let actual = System.Text.StringBuilder()
  let coroutine = Coroutine()
  coroutine.Put(fun yield' -> cont {
    actual.Append("A") |> ignore
    do! yield'()
    actual.Append("B") |> ignore
    do! yield'()
    actual.Append("C") |> ignore
  })
  coroutine.Put(fun yield' -> cont {
    actual.Append("1") |> ignore
    do! yield'()
    actual.Append("2") |> ignore
  })
  coroutine.Run()
  actual.ToString() |> should equal "A1B2C"
