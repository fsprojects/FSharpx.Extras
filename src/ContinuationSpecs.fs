module ContinuationTests
open System
open FSharp.Monad.Continuation
open NaturalSpec

let c n = cont { return n }
let addSomeNumbers =
  cont {
    let! x = c 6
    let! y = c 7
    return x + y
  }

[<Scenario>]
let ``When adding 6 to 7 and applying a continuation to convert to string and replace 1 with a, it should return a3``() =
  let ``running the continuation with`` f m =
    printMethod ""
    runCont m f 
  Given addSomeNumbers
  |> When ``running the continuation with`` (fun x -> x.ToString().Replace('1', 'a'))
  |> It should equal "a3"
  |> Verify