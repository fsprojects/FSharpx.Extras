// -------------------------------------------------------------------------------
// F# async extensions (CircularBuffer.fsx)
// (c) Tomas Petricek & Ryan Riley, 2011-2012, Available under Apache 2.0 license.
// -------------------------------------------------------------------------------

// This example demonstrates how to use `CircularBuffer`, `CircularQueueAgent`, and `CircularStream`.
// The agent implements producer/consumer concurrent pattern.

#r "System.dll"
#r @"..\build\FSharpx.Core.dll"

open System
open System.Diagnostics
open FSharp.Control
open FSharp.IO
open FSharpx.Collections.Mutable

// [snippet: Using CircularBuffer]
let queue = CircularBuffer(5)

let stopwatch = Stopwatch.StartNew()

// Printing from a queue 1..5
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
queue.Enqueue(4)
queue.Enqueue(5)
assert ([|1;2;3;4;5|] = queue.Dequeue(5))

// Printing from a queue 1..8, twice
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
queue.Enqueue(4)
queue.Enqueue(5) // <---
queue.Enqueue(6)
queue.Enqueue(7)
queue.Enqueue(8)
queue.Enqueue(1)
queue.Enqueue(2) // <---
queue.Enqueue(3)
queue.Enqueue(4)
queue.Enqueue(5)
queue.Enqueue(6)
queue.Enqueue(7) // <---
queue.Enqueue(8)
assert ([|4;5;6;7;8|] = queue.Dequeue(5))

// Printing from a queue 1..5
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
queue.Enqueue(4)
queue.Enqueue(5)
assert ([|1;2;3|] = queue.Dequeue(3))

// Clear out the rest
queue.Dequeue(2)

// Printing from a queue 1..3
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
assert ([|1;2;3|] = queue.Dequeue(3))

// Printing from a queue 1..8 and dequeue 5, then enqueue 1..3 and dequeue 3
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
queue.Enqueue(4)
queue.Enqueue(5) // <---
queue.Enqueue(6)
queue.Enqueue(7)
queue.Enqueue(8)
assert ([|4;5;6;7;8|] = queue.Dequeue(5))
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
assert ([|1;2;3|] = queue.Dequeue(3))

printfn "Enqueue(value) tests passed in %d ms" stopwatch.ElapsedMilliseconds

stopwatch.Reset()
stopwatch.Start()

// Printing from a queue 1..5
queue.Enqueue([|1;2;3;4;5|])
assert ([|1;2;3;4;5|] = queue.Dequeue(5))

// Printing from a queue 1..8, twice
let error = ref Unchecked.defaultof<Exception>
try
  try
    queue.Enqueue([|1;2;3;4;5;6;7;8;1;2;3;4;5;6;7;8|])
  with e -> error := e
finally
  assert (!error <> null)

queue.Enqueue([|1;2;3;4;5|])
queue.Enqueue([|6;7;8|])
queue.Enqueue([|1;2;3;4;5|])
queue.Enqueue([|6;7;8|])
assert ([|4;5;6;7;8|] = queue.Dequeue(5))

// Printing from a queue 1..5
queue.Enqueue([|1;2;3;4;5|])
assert ([|1;2;3|] = queue.Dequeue(3))

// Clear out the rest
queue.Dequeue(2)

// Printing from a queue 1..3
queue.Enqueue([|1;2;3|])
assert ([|1;2;3|] = queue.Dequeue(3))

// Printing from a queue 1..8 and dequeue 5, then enqueue 1..3 and dequeue 3
queue.Enqueue([|1;2;3;4;5|])
queue.Enqueue([|6;7;8|])
assert ([|4;5;6;7;8|] = queue.Dequeue(5))
queue.Enqueue([|1;2;3|])
assert ([|1;2;3|] = queue.Dequeue(3))

printfn "Enqueue(array) tests passed in %d ms" stopwatch.ElapsedMilliseconds

stopwatch.Reset()
stopwatch.Start()

// Consider a large array with various, incoming array segments.
let source =
    [| 1;2;3;4;5
       1;2;3;4;5;6;7;8;1;2;3;4;5;6;7;8
       1;2;3;4;5
       1;2;3
       1;2;3;4;5;6;7;8
       1;2;3 |]

let incoming =
    let generator =
        seq { yield ArraySegment<_>(source,0,5)
              yield ArraySegment<_>(source,5,5)
              yield ArraySegment<_>(source,10,3)
              yield ArraySegment<_>(source,13,5)
              yield ArraySegment<_>(source,18,3)
              yield ArraySegment<_>(source,21,5)
              yield ArraySegment<_>(source,26,3)
              yield ArraySegment<_>(source,29,5)
              yield ArraySegment<_>(source,34,3)
              yield ArraySegment<_>(source,37,3) } 
    in generator.GetEnumerator()

let enqueueNext() =
    incoming.MoveNext() |> ignore
    queue.Enqueue(incoming.Current)

// Printing from a queue 1..5
enqueueNext()
assert ([|1;2;3;4;5|] = queue.Dequeue(5))

// Printing from a queue 1..8, twice
enqueueNext()
enqueueNext()
enqueueNext()
enqueueNext()
assert ([|4;5;6;7;8|] = queue.Dequeue(5))

// Printing from a queue 1..5
enqueueNext()
assert ([|1;2;3|] = queue.Dequeue(3))

// Clear out the rest
queue.Dequeue(2)

// Printing from a queue 1..3
enqueueNext()
assert ([|1;2;3|] = queue.Dequeue(3))

// Printing from a queue 1..8 and dequeue 5, then enqueue 1..3 and dequeue 3
enqueueNext()
enqueueNext()
assert ([|4;5;6;7;8|] = queue.Dequeue(5))
enqueueNext()
assert ([|1;2;3|] = queue.Dequeue(3))

printfn "Enqueue(array) tests passed in %d ms" stopwatch.ElapsedMilliseconds

let data = [|1;2;3;4;5|]
queue.Enqueue(data)
assert ((data |> Array.toList) = (queue |> Seq.toList))    

printfn "Seq.toList matches enqueued data."

// [/snippet]

stopwatch.Reset()
stopwatch.Start()
// [snippet: Using CircularQueueAgent]
let buffer = new CircularQueueAgent<int>(3)

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
    buffer.Enqueue([|i|])
    printfn "Added %d" i }
|> Async.Start

async { 
  while true do
    // Sleep for some time and then get value
    do! Async.Sleep(consumerTimeout)
    let! v = buffer.AsyncDequeue(1)
    printfn "Got %d" v.[0] }
|> Async.Start
// [/snippet]
printfn "CircularQueueAgent.Enqueue(array) tests passed in %d ms" stopwatch.ElapsedMilliseconds

stopwatch.Reset()
stopwatch.Start()
// [snippet: Using CircularQueueAgent with AsyncEnqueue]
async { 
  for i in 0 .. 10 do 
    // Sleep for some time and then add value
    do! Async.Sleep(producerTimeout)
    do! buffer.AsyncEnqueue([|i|])
    printfn "Added %d" i }
|> Async.Start

async { 
  while true do
    // Sleep for some time and then get value
    do! Async.Sleep(consumerTimeout)
    let! v = buffer.AsyncDequeue(1)
    printfn "Got %d" v.[0] }
|> Async.Start
// [/snippet]
printfn "CircularQueueAgent.AsyncEnqueue(array) tests passed in %d ms" stopwatch.ElapsedMilliseconds

stopwatch.Reset()
stopwatch.Start()
// [snippet: Using CircularStream]
let stream = new CircularStream(3)

async { 
  for i in 0uy .. 10uy do 
    // Sleep for some time and then add value
    do! Async.Sleep(producerTimeout)
    do! stream.AsyncWrite([|i|], 0, 1)
    printfn "Wrote %d" i }
|> Async.Start

async { 
  let buffer = Array.zeroCreate<byte> 1
  while true do
    // Sleep for some time and then get value
    do! Async.Sleep(consumerTimeout)
    let! v = stream.AsyncRead(buffer, 0, 1)
    printfn "Read %d bytes with value %A" v buffer.[0] }
|> Async.Start
// [/snippet]
printfn "CircularStream.AsyncWrite(array) tests passed in %d ms" stopwatch.ElapsedMilliseconds
