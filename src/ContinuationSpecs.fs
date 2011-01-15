module ContinuationTests
open System
open FSharp.Monad
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
    m f 
  Given (runCont addSomeNumbers)
  |> When ``running the continuation with`` (fun x -> x.ToString().Replace('1', 'a'))
  |> It should equal "a3"
  |> Verify

let ``sum list`` l =
  printMethod ""
  let rec sum l = cont {
    let! result = callCC (fun exit1 -> cont {
      match l with
      | [] -> return 0
      | h::t when h = 2 -> return! exit1 42
      | h::t -> let! r = sum t
                return h + r })
    return result }
  runCont (sum l) (id)

[<Scenario>]
let ``When summing a list without a 2 via callCC it should return 8``() =
  Given [1;1;3;3]
  |> When ``sum list``
  |> It should equal 8
  |> Verify

[<Scenario>]
let ``When summing a list containing 2 via callCC it should return 43``() =
  Given [1;2;3]
  |> When ``sum list``
  |> It should equal 43
  |> Verify

/// Call/CC scenario, taken from <see href="http://www.haskell.org/all_about_monads/html/contmonad.html"/>.
(*
[<Scenario>]
let ``CallCC should allow escapes from continuations``() =
  let callCCTest n = cont {
    let str = callCC (fun exit1 -> cont {
      if n < 10 then return! exit1 (n.ToString()) else
      let ns = (n/2).ToString().ToCharArray() |> Array.map (fun c -> Convert.ToInt32(c))
      let n' = callCC (fun exit2 -> cont {
        if ns.Length < 3 then return! exit2 ns.Length
        elif ns.Length < 5 then return! exit2 n
        elif ns.Length < 7 then return! cont {
          let ns' = ns |> Array.rev |> Array.map (fun i -> Convert.ToChar(i))
          let result = ns' |> Seq.skipWhile ((=) '0')
          return! exit1 (result.ToString()) }
        return Array.sum ns })
      return "(ns = " + ns.ToString() + ") " + n' })
    return "Answer: " + str }

  let ``running with callcc`` n test =
    printMethod ""
    runCont (test n) id

  Given (callCCTest)
  |> When ``running with callcc`` 1
  |> It should equal "1"
  |> Verify
*)