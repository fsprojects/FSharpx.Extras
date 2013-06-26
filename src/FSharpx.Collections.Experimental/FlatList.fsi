namespace FSharpx.Collections.Experimental 

[<Struct>]
type FlatList<'T> =

    val internal array : 'T[]
    interface System.Collections.Generic.IEnumerable<'T>
    interface System.Collections.IEnumerable

    /// O(1). Returns flatlist element at the index.
    member Item : int -> 'T with get

    /// O(1). Returns the number of items in the flatlist.
    member Length : int

    /// O(1). Returns true if the flatlist has no elements.
    member IsEmpty : bool

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FlatList =

    /// O(n). Creates a flatlist that contains the elements of one flatlist followed by the elements of another flatlist.
    val append : FlatList<'T> -> FlatList<'T> -> FlatList<'T> 

    ///O(n). Applies the supplied function to each element of a flatlist, concatenates the results, and returns the combined flatlist.
    val collect : ('T -> FlatList<'T>) -> FlatList<'T> -> FlatList<'T>

    /// O(n). Creates a flatlist that contains the elements of each of the supplied sequence of flatlists.
    val concat : FlatList<'T []> -> FlatList<'T>

    /// O(1). Returns flatlist of no elements.
    [<GeneralizableValue>]
    val empty<'T> : FlatList<'T> 

    /// O(n) worst case. Tests whether any element of a flatlist satisfies the supplied predicate.
    val exists : ('T -> bool) -> FlatList<'T> -> bool

    /// O(n). Returns a flatlist that contains only the elements of the supplied flatlist for which the supplied condition returns true.
    val filter : ('T -> bool) -> FlatList<'T> -> FlatList<'T>

    /// O(n). Applies a function to each element of a flatlist from left to right (first to last), threading an accumulator argument through the computation. 
    val fold : ('State -> 'T -> 'State) -> 'State -> FlatList<'T> -> 'State

    /// O(n). Applies a function to pairs of elements from two supplied flatlists, left-to-right, threading an accumulator argument through the computation. The two input flatlists must have the same lengths; otherwise, ArgumentException is raised.
    val fold2 : ('State -> 'T1 -> 'T2 -> 'State) -> 'State -> FlatList<'T1> -> FlatList<'T2> -> 'State 
      
    /// O(n). Applies a function to each element of a flatlist from right to left (last to first), threading an accumulator argument through the computation.
    val foldBack : ('T -> 'State -> 'State) -> FlatList<'T> -> 'State -> 'State

    /// O(n). Applies a function to pairs of elements from two supplied flatlists, right-to-left, threading an accumulator argument through the computation. The two input flatlists must have the same lengths; otherwise, ArgumentException is raised.
    val foldBack2 : ('T1 -> 'T2 -> 'State -> 'State) -> FlatList<'T1> -> FlatList<'T2> -> 'State -> 'State

    /// O(n). Tests whether all elements of a flatlist satisfy the supplied condition.
    val forall : ('T -> bool) -> FlatList<'T> -> bool

    /// O(n). Tests whether all corresponding elements of two supplied flatlists satisfy a supplied condition.
    val forall2 : ('T1 -> 'T2 -> bool) -> FlatList<'T1> -> FlatList<'T2> -> bool

    /// O(n). Uses a supplied function to create a flatlist of the supplied dimension.
    val init : int -> f:(int -> 'T) -> FlatList<'T>
    
    /// O(1). O(1). Returns true if the flatlist has no elements.
    val isEmpty : FlatList<'T> -> bool
    
    /// O(n). Applies the supplied function to each element of a flatlist.
    val iter : ('T -> unit) -> FlatList<'T> -> unit

    /// O(n). Applies the supplied function to a pair of elements from matching indexes in two flatlists, also passing the index of the elements. The two flatlists must have the same lengths; otherwise, an ArgumentException is raised.
    val iter2 : ('T1 -> 'T2 -> unit) -> FlatList<'T1> -> FlatList<'T2> -> unit

    /// O(n). Applies the supplied function to each element of a flatlist. The integer passed to the function indicates the index of the element.
    val iteri : (int -> 'T -> unit) -> FlatList<'T> -> unit

    /// O(1). Returns the number of items in the flatlist.
    val length : FlatList<'T> -> int

    /// O(n). Creates a flatlist whose elements are the results of applying the supplied function to each of the elements of a supplied flatlist.
    val map : ('T1 -> 'T2) -> FlatList<'T1> -> FlatList<'T2>

    /// O(n). Creates a flatlist whose elements are the results of applying the supplied function to the corresponding elements of two supplied flatlists. The two input flatlists must have the same lengths; otherwise, ArgumentException is raised.
    val map2 : ('T1 -> 'T2 -> 'T3) -> FlatList<'T1> -> FlatList<'T2> -> FlatList<'T3>

    /// O(n). Creates a flatlist whose elements are the results of applying the supplied function to each of the elements of a supplied flatlist. An integer index passed to the function indicates the index of the element being transformed.
    val mapi : (int -> 'T1 -> 'T2) -> FlatList<'T1> -> FlatList<'T2>

    /// O(n). Creates a flatlist from the supplied list.
    val ofList : 'T list -> FlatList<'T>
      
    /// O(n). Creates a flatlist from the supplied enumerable object.
    val ofSeq : seq<'T> -> FlatList<'T>

    /// O(n). Splits a flatlist into two flatlists, one containing the elements for which the supplied condition returns true, and the other containing those for which it returns false.
    val partition : ('T -> bool) -> FlatList<'T> -> FlatList<'T> * FlatList<'T>

    /// O(1). True if the flatlists are reference-equal, false otherwise
    val physicalEquality : FlatList<'T> -> FlatList<'T> -> bool

    /// O(n). Reverses the order of the elements in a supplied array.
    val rev : FlatList<'T> -> FlatList<'T>

    /// O(1). Returns a flatlist of one element.
    val singleton : 'T -> FlatList<'T>

    /// O(n). Returns the sum of the elements in the flatlist.
    val sum : FlatList<int> -> int

    /// O(n). Returns the sum of the results generated by applying a function to each element of a flatlist.
    val sumBy : ('T -> int) -> FlatList<'T> -> int

    /// O(n). Converts the supplied flatlist to a list.
    val toList : FlatList<'T> -> 'T list

    /// O(n). Converts the supplied flatlist of tuple pairs to a map.
    val toMap : FlatList<'Key * 'T> -> Map<'Key,'T> when 'Key : comparison

    /// O(n). Returns the first element in the supplied flatlist for which the supplied function returns true. Returns None if no such element exists.
    val tryFind : ('T -> bool) -> FlatList<'T> -> 'T option

    /// O(n). Splits a flatlist of tuple pairs into a tuple of two flatlists.
    val unzip : FlatList<'T1 * 'T2> -> FlatList<'T1> * FlatList<'T2>

    /// O(n). Combines two flatlists into a flatlist of tuples that have two elements. The two flatlists must have equal lengths; otherwise, ArgumentException is raised.
    val zip : FlatList<'T1> -> FlatList<'T2> -> FlatList<'T1 * 'T2>