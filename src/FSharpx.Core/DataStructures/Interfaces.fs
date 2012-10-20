[<AutoOpen>]
module FSharpx.DataStructures.Interfaces

type IVector<'a> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'a>

    /// Returns the value at the index. If the index is out of bounds it throws an exception.
    abstract member Item  : int -> 'a with get

    /// Returns a new vector with the element 'added' at the end.
    abstract member Conj : 'a -> IVector<'a>

    /// Returns the number of items in the collection.
    abstract member Count : unit -> int

    /// Returns the last element in the vector. If the vector is empty it throws an exception.
    abstract member Peek : unit -> 'a

    /// Returns a new vector without the last item. If the collection is empty it throws an exception.
    abstract member Pop : unit -> IVector<'a>

    /// Returns a new vector that contains the given value at the index. Note - index must be <= vector.Count.
    abstract member AssocN : int*'a -> IVector<'a>

type IDeque<'a> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'a>
    
    ///returns a new deque with the element added to the beginning
    abstract member Cons : 'a -> IDeque<'a>

    ///returns the count of elememts
    abstract member Count : int with get

    ///returns the first element
    abstract member Head : 'a with get

    ///returns option first element
    abstract member TryGetHead : 'a option with get

    ///returns a new deque of the elements before the last element
    abstract member Init : IDeque<'a> with get

    ///returns option deque of the elements before the last element
    abstract member TryGetInit : IDeque<'a> option with get

    ///returns true if the deque has no elements
    abstract member IsEmpty : bool with get

    ///returns the last element
    abstract member Last : 'a with get

    ///returns option last element
    abstract member TryGetLast : 'a option with get

    ///returns the count of elememts
    abstract member Length : int with get

    ///returns element by index
    abstract member Lookup : int -> 'a

    ///returns option element by index
    abstract member TryLookup : int -> 'a option

    ///returns deque with element removed by index
    abstract member Remove : int -> IDeque<'a>

    //returns option deque with element removed by index
    abstract member TryRemove : int -> IDeque<'a> option

    ///returns deque reversed
    abstract member Rev : IDeque<'a> with get

    ///returns a new deque with the element added to the end
    abstract member Snoc : 'a -> IDeque<'a>

    ///returns a new deque of the elements trailing the first element
    abstract member Tail : IDeque<'a> with get

    ///returns option deque of the elements trailing the first element
    abstract member TryGetTail : IDeque<'a> option with get

    ///returns the first element and tail
    abstract member Uncons : 'a * IDeque<'a> with get

    //returns the option first element and tail
    abstract member TryUncons : ('a * IDeque<'a>) option with get

    ///returns init and the last element
    abstract member Unsnoc : IDeque<'a> * 'a with get

    ///returns option init and the last element
    abstract member TryUnsnoc : (IDeque<'a> * 'a) option with get

    ///returns deque with element updated by index
    abstract member Update : int -> 'a -> IDeque<'a>

    ///returns option deque with element updated by index
    abstract member TryUpdate : int -> 'a -> IDeque<'a> option

type IHeap<'a when 'a : comparison> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'a>

    ///returns the count of elememts
    abstract member Count : int with get

    ///returns the min or max element
    abstract member Head : 'a with get

    ///returns option first min or max element
    abstract member TryGetHead : 'a option with get

    ///returns true if the heap has no elements
    abstract member IsEmpty : bool with get

    ///returns true if the heap has max element at head
    abstract member IsMaximalist : bool with get

    ///returns the count of elememts
    abstract member Length : int with get

type IHeap<'c, 'a when 'c :> IHeap<'c, 'a> and 'a : comparison> =
    inherit IHeap<'a>

    ///returns a new heap with the element inserted
    abstract member Insert : 'a -> 'c

    ///returns heap from merging two heaps, both must have same isMaximalist
    abstract member Merge : 'c -> 'c

    ///returns heap option from merging two heaps
    abstract member TryMerge : 'c -> 'c option

    ///returns a new heap of the elements trailing the head
    abstract member Tail : 'c with get

    ///returns option heap of the elements trailing the head
    abstract member TryGetTail : 'c option with get

    ///returns the head element and tail
    abstract member Uncons : 'a * 'c with get

    ///returns option head element and tail
    abstract member TryUncons : ('a * 'c) option with get

type IQueue<'a> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'a>
 
    ///returns the count of elememts
    abstract member Count :int with get

    ///returns the first element
    abstract member Head :'a with get

    ///returns option first element
    abstract member TryGetHead :'a option with get

    ///returns true if the queue has no elements
    abstract member IsEmpty :bool with get

    ///returns the count of elememts
    abstract member Length :int with get

    ///returns a new queue with the element added to the end
    abstract member Snoc : 'a -> IQueue<'a>

    ///returns a new queue of the elements trailing the first element
    abstract member Tail : IQueue<'a> with get

    ///returns option queue of the elements trailing the first element
    abstract member TryGetTail : IQueue<'a> option with get

    ///returns the first element and tail
    abstract member Uncons : 'a * IQueue<'a> with get

    //returns the option first element and tail
    abstract member TryUncons : ('a * IQueue<'a>) option with get

type IRandomAccessList<'a> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'a>
    
    ///returns a new random access list with the element added to the beginning
    abstract member Cons : 'a -> IRandomAccessList<'a>

    ///returns the count of elememts
    abstract member Count : int with get

    ///returns the first element
    abstract member Head : 'a with get

    ///returns option first element 
    abstract member TryGetHead : 'a option with get

    ///returns true if the random access list has no elements
    abstract member IsEmpty : bool with get

    ///returns the count of elememts
    abstract member Length : int with get

    ///returns element by index
    abstract member Lookup : int -> 'a

    ///returns option element by index
    abstract member TryLookup : int -> 'a option

    ///returns random access list reversed
    abstract member Rev : IRandomAccessList<'a> with get

    ///returns a new random access list of the elements trailing the first element
    abstract member Tail : IRandomAccessList<'a> with get

    ///returns a option random access list of the elements trailing the first element
    abstract member TryGetTail : IRandomAccessList<'a> option with get

    ///returns the first element and tail
    abstract member Uncons : 'a * IRandomAccessList<'a> with get

    ///returns the option first element and tail
    abstract member TryUncons : ('a * IRandomAccessList<'a>) option with get

    ///returns random access list with element updated by index
    abstract member Update : int -> 'a -> IRandomAccessList<'a>

    ///returns option random access list with element updated by index
    abstract member TryUpdate : int -> 'a -> IRandomAccessList<'a> option