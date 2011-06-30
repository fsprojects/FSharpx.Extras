module FSharp.Monad.Tests.ContinuationTest

open System
open FSharp.Monad.Continuation
open NUnit.Framework
open FsUnit

let c n = cont { return n }
let addSomeNumbers = cont {
  let! x = c 6
  let! y = c 7
  return x + y }

[<Test>]
let ``When adding 6 to 7 and applying a continuation to convert to string and replace 1 with a, it should return a3``() =
  let run f m = m f 
  addSomeNumbers (fun x -> x.ToString().Replace('1', 'a')) |> should equal "a3"

(* Test callCC *)
let sum l =
  let rec sum l = cont {
    let! result = callCC (fun exit1 -> cont {
      match l with
      | [] -> return 0
      | h::t when h = 2 -> return! exit1 42
      | h::t -> let! r = sum t
                return h + r })
    return result }
  (sum l) (id)

[<Test>]
let ``When summing a list without a 2 via callCC it should return 8``() =
  sum [1;1;3;3] |> should equal 8

[<Test>]
let ``When summing a list containing 2 via callCC it should return 43``() =
  sum [1;2;3] |> should equal 43

/// Call/CC scenario, taken from <see href="http://www.haskell.org/all_about_monads/html/contmonad.html"/>.
(*
[<Test>]
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

  let run n test = runCont (test n) id

  run callCCTest 1 |> should equal "1"
*)