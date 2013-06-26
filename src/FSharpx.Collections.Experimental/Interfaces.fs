// Copyright 2010-2013, as indicated in README.md in the root directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 (the "License")


[<AutoOpen>]
module FSharpx.Collections.Experimental.Interfaces

type IVector<'T> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'T>

    /// Returns the value at the index. If the index is out of bounds it throws an exception.
    abstract member Item  : int -> 'T with get

    /// Returns a new vector with the element 'added' at the end.
    abstract member Conj : 'T -> IVector<'T>

    /// Returns the number of items in the collection.
    abstract member Count : unit -> int

    /// Returns the last element in the vector. If the vector is empty it throws an exception.
    abstract member Peek : unit -> 'T

    /// Returns a new vector without the last item. If the collection is empty it throws an exception.
    abstract member Pop : unit -> IVector<'T>

    /// Returns a new vector that contains the given value at the index. Note - index must be <= vector.Count.
    abstract member AssocN : int*'T -> IVector<'T>

type IDeque<'T> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'T>
    
    ///returns a new deque with the element added to the beginning
    abstract member Cons : 'T -> IDeque<'T>

    ///returns the count of elememts
    abstract member Count : int with get

    ///returns the first element
    abstract member Head : 'T with get

    ///returns option first element
    abstract member TryGetHead : 'T option with get

    ///returns a new deque of the elements before the last element
    abstract member Init : IDeque<'T> with get

    ///returns option deque of the elements before the last element
    abstract member TryGetInit : IDeque<'T> option with get

    ///returns true if the deque has no elements
    abstract member IsEmpty : bool with get

    ///returns the last element
    abstract member Last : 'T with get

    ///returns option last element
    abstract member TryGetLast : 'T option with get

    ///returns the count of elememts
    abstract member Length : int with get

    ///returns element by index
    abstract member Lookup : int -> 'T

    ///returns option element by index
    abstract member TryLookup : int -> 'T option

    ///returns deque with element removed by index
    abstract member Remove : int -> IDeque<'T>

    //returns option deque with element removed by index
    abstract member TryRemove : int -> IDeque<'T> option

    ///returns deque reversed
    abstract member Rev : IDeque<'T> with get

    ///returns a new deque with the element added to the end
    abstract member Snoc : 'T -> IDeque<'T>

    ///returns a new deque of the elements trailing the first element
    abstract member Tail : IDeque<'T> with get

    ///returns option deque of the elements trailing the first element
    abstract member TryGetTail : IDeque<'T> option with get

    ///returns the first element and tail
    abstract member Uncons : 'T * IDeque<'T> with get

    //returns the option first element and tail
    abstract member TryUncons : ('T * IDeque<'T>) option with get

    ///returns init and the last element
    abstract member Unsnoc : IDeque<'T> * 'T with get

    ///returns option init and the last element
    abstract member TryUnsnoc : (IDeque<'T> * 'T) option with get

    ///returns deque with element updated by index
    abstract member Update : int -> 'T -> IDeque<'T>

    ///returns option deque with element updated by index
    abstract member TryUpdate : int -> 'T -> IDeque<'T> option

type IHeap<'T when 'T : comparison> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'T>

    ///returns the count of elememts
    abstract member Count : unit -> int

    ///returns the min or max element
    abstract member Head : unit -> 'T 

    ///returns option first min or max element
    abstract member TryGetHead : unit -> 'T option

    ///returns true if the heap has no elements
    abstract member IsEmpty : bool with get

    ///returns true if the heap has max element at head
    abstract member IsDescending : bool with get

    ///returns the count of elememts
    abstract member Length : unit -> int

type IHeap<'c, 'T when 'c :> IHeap<'c, 'T> and 'T : comparison> =
    inherit IHeap<'T>

    ///returns a new heap with the element inserted
    abstract member Insert : 'T -> 'c

    ///returns heap from merging two heaps, both must have same isDescending
    abstract member Merge : 'c -> 'c

    ///returns heap option from merging two heaps
    abstract member TryMerge : 'c -> 'c option

    ///returns a new heap of the elements trailing the head
    abstract member Tail : unit -> 'c 

    ///returns option heap of the elements trailing the head
    abstract member TryGetTail : unit -> 'c option 

    ///returns the head element and tail
    abstract member Uncons : unit -> ('T * 'c) 

    ///returns option head element and tail
    abstract member TryUncons : unit -> ('T * 'c) option

type IQueue<'T> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'T>
 
    ///returns the count of elememts
    abstract member Count : unit -> int

    ///returns the first element
    abstract member Head :'T with get

    ///returns option first element
    abstract member TryGetHead :'T option with get

    ///returns true if the queue has no elements
    abstract member IsEmpty :bool with get

    ///returns the count of elememts
    abstract member Length : unit -> int

    ///returns a new queue with the element added to the end
    abstract member Snoc : 'T -> IQueue<'T>

    ///returns a new queue of the elements trailing the first element
    abstract member Tail : IQueue<'T> with get

    ///returns option queue of the elements trailing the first element
    abstract member TryGetTail : IQueue<'T> option with get

    ///returns the first element and tail
    abstract member Uncons : 'T * IQueue<'T> with get

    //returns the option first element and tail
    abstract member TryUncons : ('T * IQueue<'T>) option with get

type IPriorityQueue<'T when 'T : comparison> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'T>

    ///returns true if the queue has no elements
    abstract member IsEmpty : bool with get

    ///returns a new queue with the element added to the end
    abstract member Insert : 'T -> IPriorityQueue<'T>

    ///returns option first element
    abstract member TryPeek : unit -> 'T option

    ///returns the first element
    abstract member Peek : unit -> 'T

    //returns the option first element and tail
    abstract member TryPop : unit -> ('T * IPriorityQueue<'T>) option

    ///returns the first element and tail
    abstract member Pop : unit -> 'T * IPriorityQueue<'T> 

type IRandomAccessList<'T> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'T>
    
    ///returns a new random access list with the element added to the beginning
    abstract member Cons : 'T -> IRandomAccessList<'T>

    ///returns the count of elememts
    abstract member Count : unit -> int

    ///returns the first element
    abstract member Head : 'T with get

    ///returns option first element 
    abstract member TryGetHead : 'T option with get

    ///returns true if the random access list has no elements
    abstract member IsEmpty : bool with get

    ///returns the count of elememts
    abstract member Length : unit -> int

    ///returns element by index
    abstract member Lookup : int -> 'T

    ///returns option element by index
    abstract member TryLookup : int -> 'T option

    ///returns random access list reversed
    abstract member Rev : unit -> IRandomAccessList<'T> 

    ///returns a new random access list of the elements trailing the first element
    abstract member Tail : IRandomAccessList<'T> with get

    ///returns a option random access list of the elements trailing the first element
    abstract member TryGetTail : IRandomAccessList<'T> option with get

    ///returns the first element and tail
    abstract member Uncons : 'T * IRandomAccessList<'T> with get

    ///returns the option first element and tail
    abstract member TryUncons : ('T * IRandomAccessList<'T>) option with get

    ///returns random access list with element updated by index
    abstract member Update : int -> 'T -> IRandomAccessList<'T>

    ///returns option random access list with element updated by index
    abstract member TryUpdate : int -> 'T -> IRandomAccessList<'T> option