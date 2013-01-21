[<AutoOpen>]
module FSharpx.Collections.IPriorityQueue

/// Priority queue is like a queue or stack data structure, but with an ascending or descending 
/// ordering of the elements. Depending on the ordering peek retrieves either the highest or lowest 
/// ordered element in the queue.
type IPriorityQueue<'T when 'T : comparison> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'T>

    ///O(1). Returns true if the queue has no elements.
    abstract member IsEmpty : bool with get

    ///O(log n) amortized time. Returns a new queue with the element added to the end.
    abstract member Insert : 'T -> IPriorityQueue<'T>

    ///O(1). Returns option first element.
    abstract member TryPeek : 'T option

    ///O(1). Returns the first element.
    abstract member Peek : 'T

    ///O(log n) amortized time. Returns the option first element and tail.
    abstract member TryPop : unit -> ('T * IPriorityQueue<'T>) option

    ///O(log n) amortized time. Returns the first element and tail.
    abstract member Pop : unit -> 'T * IPriorityQueue<'T> 