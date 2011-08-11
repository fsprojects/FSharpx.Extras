// ----------------------------------------------------------------------------
// F# async extensions (AutoCancel.fsx)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

// This example demonstrates how to use 'Async.Cache' and 'AsyncSeq.cache'

#r "..\\bin\\FSharp.AsyncExtensions.dll"
open FSharp.Control

// The Async.Cache combinator makes it possible to create asynchronous
// workflow that caches the result and performs computation only once
// when called multiple times.
let op = 
  async { // Will be printed just once
          printfn "Evaluating..."
          return 42 }
  |> Async.Cache

Async.RunSynchronously(op)
Async.RunSynchronously(op)


// The AsyncSeq.cache combinator has similar effect - the asynchronous
// sequence can be used multiple times, but it will evaluate just once
let asq = 
  asyncSeq { for i in 0 .. 10 do
               printfn "Generating %d..." i
               yield i }
  |> AsyncSeq.cache

AsyncSeq.iter (printfn "Consuming %d") asq
|> Async.RunSynchronously