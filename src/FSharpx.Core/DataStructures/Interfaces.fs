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

type IDequeue<'a> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'a>
    
    ///returns a new deque with the element added to the beginning
    abstract member Cons : 'a -> IDequeue<'a>

    ///returns the first element
    abstract member Head : unit -> 'a

    ///returns a new deque of the elements before the last element
    abstract member Init : unit -> IDequeue<'a>

    ///returns true if the deque has no elements
    abstract member IsEmpty : unit -> bool

    ///returns the last element
    abstract member Last : unit -> 'a

    ///returns the count of elememts
    abstract member Length : unit -> int

    ///returns a new deque with the element added to the end
    abstract member Snoc : 'a -> IDequeue<'a>

    ///returns a new deque of the elements trailing the first element
    abstract member Tail : unit -> IDequeue<'a>

    ///returns the first element and tail
    abstract member Uncons : unit -> 'a * IDequeue<'a>

    ///returns init and the last element
    abstract member Unsnoc : unit -> IDequeue<'a> * 'a