// ----------------------------------------------------------------------------
// F# async extensions (BlockingQueue.fsx)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

// This example demonstrates how to use 'BlockingAgent'
// The agent implements producer/consumer concurrent pattern.

#r "..\\bin\\FSharp.AsyncExtensions.dll"
open FSharp.Control

let buffer = new BlockingQueueAgent<int>(3)

// The sample uses two workflows that add/take elements
// from the buffer with the following timeouts. When the producer
// timout is larger, consumer will be blocked. Otherwise, producer
// will be blocked.
let producerTimeout = 500
let consumerTimeout = 1000

async { 
  for i in 0 .. 10 do 
    // Sleep for some time and then add value
    do! Async.Sleep(producerTimeout)
    do! buffer.AsyncAdd(i)
    printfn "Added %d" i }
|> Async.Start

async { 
  while true do
    // Sleep for some time and then get value
    do! Async.Sleep(consumerTimeout)
    let! v = buffer.AsyncGet()
    printfn "Got %d" v }
|> Async.Start