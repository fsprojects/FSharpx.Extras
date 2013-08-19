namespace FSharpx.DataStructures

#nowarn "44"
open FSharpx.DataStructures.PairingHeap

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
module HeapPriorityQueue =
    let empty<'a when 'a : comparison> maxQueue = PairingHeap.empty maxQueue :> IPriorityQueue<'a>

    let inline isEmpty (pq:IPriorityQueue<'a>) = pq.IsEmpty

    let inline insert element (pq:IPriorityQueue<'a>) = pq.Insert element

    let inline tryPeek (pq:IPriorityQueue<'a>) = pq.TryPeek()

    let inline peek (pq:IPriorityQueue<'a>) = pq.Peek()

    let inline tryPop (pq:IPriorityQueue<'a>) = pq.TryPop()

    let inline pop (pq:IPriorityQueue<'a>) = pq.Pop()