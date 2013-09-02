namespace FSharpx.Collections

/// Vector is an ordered linear structure implementing the inverse of the List signature, 
/// (last, initial, conj) in place of (head, tail, cons). Indexed lookup or update 
/// (returning a new immutable instance of Vector) of any element is O(log32n). Length is O(1). 
/// Ordering is by insertion history.
/// Original F# adaptation from the clojure implementation by Steffen Forkmann.
[<Class>]
type Vector<[<EqualityConditionalOn>]'T when 'T : equality> =

    interface System.Collections.Generic.IEnumerable<'T>
    interface System.Collections.IEnumerable

    /// O(1). Returns a new vector with the element added at the end.
    member Conj : 'T -> Vector<'T>
         
    /// O(n). Returns a new vector without the last item. If the collection is empty it throws an exception.
    member Initial : Vector<'T>

    /// O(n). Returns option vector without the last item.
    member TryInitial : Vector<'T> option

    /// O(1). Returns true if the vector has no elements.
    member IsEmpty : bool

    /// O(log32n). Returns vector element at the index.
    member Item : int -> 'T with get

    /// O(1). Returns the last element in the vector. If the vector is empty it throws an exception.
    member Last : 'T

    /// O(1). Returns option last element in the vector.
    member TryLast : 'T option

    /// O(1). Returns the number of items in the vector.
    member Length : int

    ///O(n). Returns random access list reversed.
    member Rev : unit -> Vector<'T>

    /// O(1). Returns tuple last element and vector without last item  
    member Unconj : Vector<'T> * 'T

    /// O(1). Returns option tuple last element and vector without last item  
    member TryUnconj : (Vector<'T> * 'T) option

    /// O(log32n). Returns a new vector that contains the given value at the index.
    member Update : int * 'T -> Vector<'T> 
            
    /// O(log32n). Returns option vector that contains the given value at the index.
    member TryUpdate : int * 'T -> Vector<'T> option

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector = 
    //pattern discriminators (active pattern)
    val (|Conj|Nil|) : Vector<'T> ->  Choice<(Vector<'T> * 'T),unit>
    
    /// O(n). Returns a new vector with the elements of the second vector added at the end.
    val append : Vector<'T> -> Vector<'T> -> Vector<'T>

    /// O(1). Returns a new vector with the element added at the end.   
    val inline conj : 'T -> Vector<'T> -> Vector<'T>

    ///O(1). Returns vector of no elements.
    [<GeneralizableValue>]
    val empty<'T when 'T : equality> : Vector<'T>

    /// O(n). Returns a state from the supplied state and a function operating from left to right.
    val inline fold : ('State -> 'T -> 'State) -> 'State -> Vector<'T> -> 'State

    /// O(m,n). Returns a seq from a vector of vectors.
    val inline flatten : Vector<Vector<'T>> -> seq<'T>

    /// O(n). Returns a state from the supplied state and a function operating from right to left.
    val inline foldBack : ('T -> 'State -> 'State) -> Vector<'T> -> 'State -> 'State

    /// O(n). Returns a vector of the supplied length using the supplied function operating on the index. 
    val init : int -> (int -> 'T) -> Vector<'T>

    /// O(n). Returns a new vector without the last item. If the collection is empty it throws an exception.
    val inline initial : Vector<'T> -> Vector<'T>

    /// O(n). Returns option vector without the last item.
    val inline tryInitial : Vector<'T> -> Vector<'T> option

    /// O(1). Returns true if the vector has no elements.
    val inline isEmpty : Vector<'T> -> bool

    /// O(1). Returns the last element in the vector. If the vector is empty it throws an exception.
    val inline last : Vector<'T> -> 'T

    /// O(1). Returns option last element in the vector.
    val inline tryLast : Vector<'T> -> 'T option

    /// O(1). Returns the number of items in the vector.
    val inline length : Vector<'T> -> int

    /// O(n). Returns a vector whose elements are the results of applying the supplied function to each of the elements of a supplied vector.
    val map : ('T -> 'T1) -> Vector<'T> -> Vector<'T1>

    /// O(log32n). Returns the value at the index. If the index is out of bounds it throws an exception.
    val inline nth : int -> Vector<'T> -> 'T

    /// O(log32(m,n)). Returns the value at the  outer index, inner index. If either index is out of bounds it throws an exception.
    val inline nthNth : int -> int -> Vector<Vector<'T>> -> 'T
 
    /// O(log32n). Returns option value at the index. 
    val inline tryNth : int -> Vector<'T> -> 'T option

    /// O(log32(m,n)). Returns option value at the indices. 
    val inline tryNthNth : int -> int -> Vector<Vector<'T>> -> 'T option

    /// O(n). Returns a vector of the seq.
    val ofSeq : seq<'T> -> Vector<'T>

    ///O(n). Returns vector reversed.
    val inline rev : Vector<'T> -> Vector<'T>

    /// O(1). Returns a new vector of one element.   
    val inline singleton : 'T -> Vector<'T>

    ///O(n). Views the given vector as a sequence.
    val inline toSeq  : Vector<'T> ->  seq<'T>

    /// O(1). Returns tuple last element and vector without last item
    val inline unconj : Vector<'T> -> Vector<'T> * 'T

    /// O(1). Returns option tuple last element and vector without last item  
    val inline tryUnconj : Vector<'T> -> (Vector<'T> * 'T) option

    /// O(log32n). Returns a new vector that contains the given value at the index. 
    val inline update : int -> 'T -> Vector<'T> -> Vector<'T>

    /// O(log32(m,n)). Returns a new vector of vectors that contains the given value at the indices. 
    val inline updateNth : int -> int -> 'T -> Vector<Vector<'T>> -> Vector<Vector<'T>>

    /// O(log32n). Returns option vector that contains the given value at the index. 
    val inline tryUpdate : int -> 'T -> Vector<'T> -> Vector<'T> option

    /// O(log32(m,n)). Returns option vector that contains the given value at the indices. 
    val inline tryUpdateNth : int -> int -> 'T -> Vector<Vector<'T>> -> Vector<Vector<'T>> option

    /// O(n). Returns a vector of vectors of given length from the seq. Result may be a jagged vector.
    val inline windowSeq : int  -> seq<'T> -> Vector<Vector<'T>>