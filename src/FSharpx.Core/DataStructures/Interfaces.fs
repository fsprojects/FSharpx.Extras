[<AutoOpen>]
module FSharpx.DataStructures.Interfaces

type IVector<'a> =
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