#r "System.dll"
open System.Diagnostics

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

    member this.Dequeue(count) =
        if length = 0 then invalidOp "Queue exhausted."
        if count > length then invalidOp "Requested count is too large."

        let dequeued = Array.concat [| for o, c in nextBuffer tail count -> buffer.[o..o+c-1] |]

        tail <- (tail + count) % bufferSize
        length <- length - count
        dequeued

    member this.Enqueue(value) =
        head <- (head + 1) % bufferSize
        buffer.[head] <- value
        if length = bufferSize then
            tail <- (tail + 1) % bufferSize
        else
            length <- length + 1

    member this.Enqueue(value: 'a[]) =
        for x in value do this.Enqueue(x)
        // NOTE: The following works, but it typically takes longer than the iteration approach above.
//        let mutable offset = 0
//        let count = value.Length
//
//        head <- (head + 1) % bufferSize
//        for x, y in nextBuffer head count do
//            Array.blit value offset buffer x y
//            offset <- offset + y
//
//        if length = bufferSize then
//            tail <- (tail + count) % bufferSize
//        else
//            let overflow = length + count - bufferSize
//            if overflow > 0 then
//                tail <- (tail + overflow) % bufferSize
//            length <- min (length + count) bufferSize

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
queue.Enqueue([|1;2;3;4;5;6;7;8;1;2;3;4;5;6;7;8|])
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
queue.Enqueue([|1;2;3;4;5;6;7;8|])
Debug.Assert([|4;5;6;7;8|] = queue.Dequeue(5))
queue.Enqueue([|1;2;3|])
Debug.Assert([|1;2;3|] = queue.Dequeue(3))

printfn "Enqueue(array) tests passed in %d ms" stopwatch.ElapsedMilliseconds
