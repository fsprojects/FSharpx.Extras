#r "System.dll"
open System
open System.Diagnostics

// NOTE: A special version for primitives would increase
// performance for primitive types, especially for I/O,
// byte-based operations.

// [snippet: Circular Buffer]
type CircularBuffer<'a> (bufferSize: int) =
    do if bufferSize <= 0 then invalidArg "bufferSize" "The bufferSize must be greater than 0."

    let buffer = Array.zeroCreate<'a> bufferSize
    let mutable head = bufferSize - 1
    let mutable tail = 0
    let mutable length = 0

    let rec nextBuffer offset count =
        seq {
            let overflow = count + offset - bufferSize
            if overflow > 0 then
                yield (offset, bufferSize - offset)
                yield! nextBuffer 0 overflow
            else
                yield (offset, count)
        }

    member __.Dequeue(count) =
        if length = 0 then invalidOp "Queue exhausted."
        if count > length then invalidOp "Requested count is too large."

        let dequeued = Array.concat [| for o, c in nextBuffer tail count -> buffer.[o..o+c-1] |]

        tail <- (tail + count) % bufferSize
        length <- length - count
        dequeued

    member __.Enqueue(value: _[], offset, count) =
        if count > bufferSize then invalidOp "Requested count is too large."

        let mutable offset = offset

        head <- (head + 1) % bufferSize
        for x, y in nextBuffer head count do
            Array.blit value offset buffer x y
            offset <- offset + y

        if length = bufferSize then
            tail <- (tail + count) % bufferSize
        else
            let overflow = length + count - bufferSize
            if overflow > 0 then
                tail <- (tail + overflow) % bufferSize
            length <- min (length + count) bufferSize

    member __.Enqueue(value: _[]) =
        __.Enqueue(value, 0, value.Length)

    member __.Enqueue(value: _[], offset) =
        __.Enqueue(value, offset, value.Length - offset)

    member __.Enqueue(value: ArraySegment<_>) =
        __.Enqueue(value.Array, value.Offset, value.Count)

    member __.Enqueue(value) =
        __.Enqueue([|value|], 0, 1)
// [/snippet]

// [snippet: Usage]
let queue = CircularBuffer(5)

let stopwatch = Stopwatch.StartNew()

// Printing from a queue 1..5
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
queue.Enqueue(4)
queue.Enqueue(5)
Debug.Assert([|1;2;3;4;5|] = queue.Dequeue(5))

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
Debug.Assert([|4;5;6;7;8|] = queue.Dequeue(5))

// Printing from a queue 1..5
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
queue.Enqueue(4)
queue.Enqueue(5)
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

// Clear out the rest
queue.Dequeue(2)

// Printing from a queue 1..3
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

// Printing from a queue 1..8 and dequeue 5, then enqueue 1..3 and dequeue 3
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
queue.Enqueue(4)
queue.Enqueue(5) // <---
queue.Enqueue(6)
queue.Enqueue(7)
queue.Enqueue(8)
Debug.Assert([|4;5;6;7;8|] = queue.Dequeue(5))
queue.Enqueue(1)
queue.Enqueue(2)
queue.Enqueue(3)
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

printfn "Enqueue(value) tests passed in %d ms" stopwatch.ElapsedMilliseconds

stopwatch.Reset()
stopwatch.Start()

// Printing from a queue 1..5
queue.Enqueue([|1;2;3;4;5|])
Debug.Assert([|1;2;3;4;5|] = queue.Dequeue(5))

// Printing from a queue 1..8, twice
let error = ref Unchecked.defaultof<Exception>
try
  try
    queue.Enqueue([|1;2;3;4;5;6;7;8;1;2;3;4;5;6;7;8|])
  with e -> error := e
finally
  Debug.Assert(!error <> null)

queue.Enqueue([|1;2;3;4;5|])
queue.Enqueue([|6;7;8|])
queue.Enqueue([|1;2;3;4;5|])
queue.Enqueue([|6;7;8|])
Debug.Assert([|4;5;6;7;8|] = queue.Dequeue(5))

// Printing from a queue 1..5
queue.Enqueue([|1;2;3;4;5|])
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

// Clear out the rest
queue.Dequeue(2)

// Printing from a queue 1..3
queue.Enqueue([|1;2;3|])
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

// Printing from a queue 1..8 and dequeue 5, then enqueue 1..3 and dequeue 3
queue.Enqueue([|1;2;3;4;5|])
queue.Enqueue([|6;7;8|])
Debug.Assert([|4;5;6;7;8|] = queue.Dequeue(5))
queue.Enqueue([|1;2;3|])
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

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
Debug.Assert([|1;2;3;4;5|] = queue.Dequeue(5))

// Printing from a queue 1..8, twice
enqueueNext()
enqueueNext()
enqueueNext()
enqueueNext()
Debug.Assert([|4;5;6;7;8|] = queue.Dequeue(5))

// Printing from a queue 1..5
enqueueNext()
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

// Clear out the rest
queue.Dequeue(2)

// Printing from a queue 1..3
enqueueNext()
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

// Printing from a queue 1..8 and dequeue 5, then enqueue 1..3 and dequeue 3
enqueueNext()
enqueueNext()
Debug.Assert([|4;5;6;7;8|] = queue.Dequeue(5))
enqueueNext()
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

printfn "Enqueue(array) tests passed in %d ms" stopwatch.ElapsedMilliseconds
// [/snippet]
