/// Originally from https://bitbucket.org/colinbul/fsharpent
namespace FSharpx.DataStructures

open System
open System.Collections
open System.Collections.Generic
open FSharpx

type RingBuffer<'a>(position : int, vals : seq<'a>) = 
    
    let buffer = vals |> Seq.toArray
    let size = buffer.Length
    let position = ref position
    let next curr = 
        (curr + 1) % size

    new(size : int) = 
        RingBuffer(0, Array.zeroCreate size)

    new(vals : seq<'a>) = 
        RingBuffer(0, vals)

    member internal x.MoveNext() = 
        position := next !position

    member x.Length 
        with get() = size

    member x.Position 
        with get() = !position

    member x.Values = 
         [|
            let pos = !position
            let count = ref -1
            while !count < (size - 1) do
                yield buffer.[next (pos + !count)]
                incr(count)
         |]

    member x.Insert(offset : int, vals : seq<'a>) =
         let values = Seq.toArray vals
         if values.Length > 0 && offset < size
         then
            let count = ref -1
            let pos = !position + offset
            let maxItems = min values.Length (size - offset)
            for v in values.[0..maxItems-1] do
                buffer.[next (pos + !count)] <- values.[!count + 1]
                incr count
    
    member x.Advance(offset : int) = 
        for indx in [1..offset] do
            buffer.[!position] <- Unchecked.defaultof<'a>
            x.MoveNext()

    member x.Normalize() = 
        buffer.[0..size - 1] <- x.Values 
        position := 0

    member x.Clone() = RingBuffer<'a>(x.Position, x.Values)

module RingBuffer =
    let create (seq: 'a seq) = new RingBuffer<'a>(seq)