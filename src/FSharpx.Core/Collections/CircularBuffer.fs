namespace FSharpx

open System
open System.Collections
open System.Collections.Generic
            
// NOTE: A special version for primitives would increase
// performance for primitive types, especially for I/O,
// byte-based operations.
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

    member this.Count = length

    member this.Dequeue(count) =
        if length = 0 then
            invalidOp "Queue exhausted."
        if count > bufferSize then
            raise <| new ArgumentOutOfRangeException("Requested count exceeds the buffer size.")

        let count = min count length
        let dequeued = Array.concat [| for o, c in nextBuffer tail count -> buffer.[o..o+c-1] |]

        tail <- (tail + count) % bufferSize
        length <- length - count
        dequeued

    member this.Enqueue(value: _[], offset, count) =
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

    member this.Enqueue(value: _[]) =
        this.Enqueue(value, 0, value.Length)

    member this.Enqueue(value: _[], offset) =
        this.Enqueue(value, offset, value.Length - offset)

    member this.Enqueue(value: ArraySegment<_>) =
        this.Enqueue(value.Array, value.Offset, value.Count)

    member this.Enqueue(value) =
        this.Enqueue([|value|], 0, 1)

    member this.GetEnumerator() =
        let rec loop() = seq {
            if length > 0 then
                yield this.Dequeue(1).[0]
            yield! loop() }
        loop().GetEnumerator()

    interface IEnumerable<'a> with
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator
