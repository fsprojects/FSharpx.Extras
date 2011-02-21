module StateTests
open FSharp.Monad.State
open NUnit.Framework
open BaseSpecs

// Simple example
let tick = state {
  let! n = getState
  do! putState (n + 1)
  return n }

[<Test>]
let ``When starting a ticker at 0, it should have a state of 0``() =
  let actual = runState tick 0
  fst actual == 0

[<Test>]
let ``When starting a ticker at 0 and ticking twice, it should have a state of 2``() =
  let test = state {
    let! _ = tick
    let! _ = tick
    return () }
  let actual = exec test 0
  actual == 2

//// Stack example
//let enqueue a = State (fun s -> ((), s @ a::[]))
//let dequeue = State (fun (hd::tl) -> (hd, tl))
//
//let workflow = state {
//  let! queue = getState
//  do! enqueue 4
//  let! hd = dequeue
//  do! enqueue (hd * 3)
//  return hd }
