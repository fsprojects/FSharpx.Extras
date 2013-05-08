namespace FSharpx.Collections

open FSharpx.Collections.Heap

module PriorityQueue =
    ///O(1). Returns a empty queue, with indicated ordering.
    let empty<'T when 'T : comparison> isDescending = Heap.empty isDescending :> IPriorityQueue<'T>

    ///O(1). Returns true if the queue has no elements.
    let inline isEmpty (pq:IPriorityQueue<'T>) = pq.IsEmpty

    ///O(log n) amortized time. Returns a new queue with the element added to the end.
    let inline insert element (pq:IPriorityQueue<'T>) = pq.Insert element

    ///O(1). Returns option first element.
    let inline tryPeek (pq:IPriorityQueue<'T>) = pq.TryPeek

    ///O(1). Returns the first element.
    let inline peek (pq:IPriorityQueue<'T>) = pq.Peek

    ///O(log n) amortized time. Returns the option first element and tail.
    let inline tryPop (pq:IPriorityQueue<'T>) = pq.TryPop()

    ///O(log n) amortized time. Returns the first element and tail.
    let inline pop (pq:IPriorityQueue<'T>) = pq.Pop()