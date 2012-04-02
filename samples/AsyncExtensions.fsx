// ----------------------------------------------------------------------------
// F# async extensions (AsyncExtensions.fsx)
// (c) Tomas Petricek, 2012, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

#r @"..\build\FSharpx.Core.dll"
open FSharp.Control

/// Returns the specified value 'v' after 'time' milliseconds
let after time v = async {
  do! Async.Sleep(time)
  // Print when returning to show how many 
  // times the function is being executed
  printfn "Returning: %A" v
  return v }

// Run this example and check how many times 'after' gets called
async { 
  let! result = async.Merge(after 1000 false, after 2000 false)
  match result with
  | true, _ -> printfn "First true"
  | _, true -> printfn "Second true"
  | a, b -> printfn "Final: %b" (a || b) } |> Async.Start
