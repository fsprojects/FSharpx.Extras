module FSharpx.DataStructures.Vector

open FSharpx

type Node = obj[]

type TransientVector<'a> (count,shift:int,root:Node,tail:obj[]) =
    let mutable count = count
    let mutable root = root
    let mutable tail = tail
    let mutable shift = shift
    
    with
        member internal this.EnsureEditable() = () // TODO:
        member this.Count = this.EnsureEditable(); count
        member internal this.Shift = shift
        member internal this.Root = root
        member internal this.Tail = tail
        member internal this.TailOff =
            if count < 32 then 0 else
            ((count - 1) >>> 5) <<< 5

        member internal this.IncCount() = count <- count + 1
        member internal this.SetRoot r = root <- r
        member internal this.SetTail t = tail <- t
        member internal this.SetShift s = shift <- s

type PersistentVector<'a> (count,shift:int,root:Node,tail:obj[]) = 
    let tailOff = 
        if count < 32 then 0 else
        ((count - 1) >>> 5) <<< 5
    with
        member this.Count = count
        member internal this.Shift = shift
        member internal this.Root = root
        member internal this.Tail = tail
        member internal this.TailOff = tailOff