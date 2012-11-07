namespace FSharpx.DataStructures

open FSharpx.DataStructures.BinomialHeap

module HeapPriorityQueue =
    let empty<'a when 'a : comparison> maxQueue = BinomialHeap<'a>.empty maxQueue :> IPriorityQueue<'a>

    let inline isEmpty (pq:IPriorityQueue<'a>) = pq.IsEmpty

    let inline insert element (pq:IPriorityQueue<'a>) = pq.Insert element

    let inline tryPeek (pq:IPriorityQueue<'a>) = pq.TryPeek()

    let inline peek (pq:IPriorityQueue<'a>) = pq.Peek()

    let inline tryPop (pq:IPriorityQueue<'a>) = pq.TryPop()

    let inline pop (pq:IPriorityQueue<'a>) = pq.Pop()