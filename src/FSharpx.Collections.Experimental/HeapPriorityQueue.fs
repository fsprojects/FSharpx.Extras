namespace FSharpx.Collections.Experimental

open FSharpx.Collections.Experimental.PairingHeap

module HeapPriorityQueue =

    let empty<'T when 'T : comparison> maxQueue = PairingHeap.empty maxQueue :> IPriorityQueue<'T>

    let inline isEmpty (pq:IPriorityQueue<'T>) = pq.IsEmpty

    let inline insert element (pq:IPriorityQueue<'T>) = pq.Insert element

    let inline tryPeek (pq:IPriorityQueue<'T>) = pq.TryPeek()

    let inline peek (pq:IPriorityQueue<'T>) = pq.Peek()

    let inline tryPop (pq:IPriorityQueue<'T>) = pq.TryPop()

    let inline pop (pq:IPriorityQueue<'T>) = pq.Pop()