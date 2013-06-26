/// Originally from https://bitbucket.org/colinbul/fsharpent
namespace FSharpx.Collections.Experimental

open System
open System.Collections
open System.Collections.Generic
open FSharpx

type RingBuffer<'T>(position:int, values:seq<'T>) =
    let buffer = values |> Seq.toArray     
    let mutable position = position

    member x.Buffer with get() = buffer
    member x.Position with get() = position and set(value) = position <- value

    new(size) = RingBuffer(0, Array.zeroCreate size)
    new(values) = RingBuffer(0, values)

    member private x.IndexOffset(i, offset) = (i + offset) % x.Buffer.Length

    member x.ToArray() =
        [|
            for i in 0 .. x.Buffer.Length - 1 do
                yield x.Buffer.[x.IndexOffset(x.Position, i)]
        |]
             
    member x.Insert(op, offset, items) =
        if offset >= 0 && offset < x.Buffer.Length then
            let values = Seq.toArray items
            let startIndex = x.IndexOffset(x.Position, offset)
            for i in 0 .. (min (x.Buffer.Length - offset) values.Length) - 1 do
                let insetIndex = x.IndexOffset(startIndex, i)
                x.Buffer.[insetIndex] <- op x.Buffer.[insetIndex] values.[i]
             
    member x.Insert(offset, items) =
        x.Insert((fun _ b -> b), offset, items)
 
    /// Tries to advance the position of the RingBuffer by the offset.
    /// Returns None if offset is negative, otherwise Some containing 
    /// the position of the RingBuffer.    
    member x.TryAdvance(offset) =
        if offset >= 0 then
            for i in 0 .. offset - 1 do
                x.Buffer.[x.IndexOffset(x.Position, i)] <- Unchecked.defaultof<'T>
            x.Position <- x.IndexOffset(x.Position, offset)
            Some(x.Position)
        else
            None

    /// Advances the position of the RingBuffer by the offset.
    /// Returns the position of the RingBuffer. Throws an ArgumentException if
    /// the offset is negative.
    member x.Advance(offset) =
        match x.TryAdvance(offset) with
        | Some(position) -> position
        | None -> invalidArg "offset" "the offset must be greater than or equal to zero"

    member x.Normalize() = 
        x.Buffer.[0 .. x.Buffer.Length - 1] <- x.ToArray() 
        x.Position <- 0

    member x.Clone() = 
        RingBuffer<'T>(x.Position, x.ToArray())

module RingBuffer =
    let create (seq: 'T seq) = new RingBuffer<'T>(seq)