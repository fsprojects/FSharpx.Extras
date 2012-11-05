namespace FSharpx.DataStructures

open FSharpx.DataStructures.BinomialHeap

type HeapPriorityQueue<'a when 'a : comparison>(heap:BinomialHeap<'a>) =
    let heap = heap
    with
        member this.IsEmpty () = isEmpty heap
        member this.Insert element = HeapPriorityQueue(heap.Insert element)
        member this.TryPeek = heap.TryGetHead
        member this.Peek = heap.Head

        member this.TryPop() = 
            match heap.TryUncons with
            | Some(element,newHeap) -> Some(element,HeapPriorityQueue(newHeap))
            | None -> None

        member this.Pop() = 
            let element,newHeap = heap.Uncons
            element,HeapPriorityQueue(newHeap)

module HeapPriorityQueue =
    let empty() = HeapPriorityQueue<'a>(BinomialHeap.empty(false))

    let inline isEmpty (pq:HeapPriorityQueue<'a>) = pq.IsEmpty()

    let inline insert element (pq:HeapPriorityQueue<'a>) = pq.Insert element

    let inline tryPeek (pq:HeapPriorityQueue<'a>) = pq.TryPeek

    let inline peek (pq:HeapPriorityQueue<'a>) = pq.Peek

    let inline tryPop (pq:HeapPriorityQueue<'a>) = pq.TryPop()

    let inline pop (pq:HeapPriorityQueue<'a>) = pq.Pop()