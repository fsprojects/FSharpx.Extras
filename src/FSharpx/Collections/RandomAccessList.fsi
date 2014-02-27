namespace FSharpx.Collections

/// RandomAccessList is an ordered linear structure implementing the List signature 
/// (head, tail, cons), as well as inspection (lookup) and update (returning a new 
/// immutable instance) of any element in the structure by index. Ordering is by insertion history.
/// Adapted from Steffen Forkmann's F# implementation of Clojure Vector..
[<Class>]
type RandomAccessList<[<EqualityConditionalOn>]'T when 'T : equality> =

    interface System.Collections.Generic.IEnumerable<'T>
    interface System.Collections.IEnumerable

    /// O(1). Returns a new random access list with the element added at the end.
    member Cons : 'T -> RandomAccessList<'T>

    /// O(1). Returns true if the random access list has no elements.
    member IsEmpty : bool

    /// O(log32n). Returns random access list element at the index.
    member Item : int -> 'T with get

    /// O(1). Returns the last element in the random access list. If the random access list is empty it throws an exception.
    member Head : 'T

    /// O(1). Returns option last element in the random access list.
    member TryHead : 'T option

    /// O(1). Returns the number of items in the random access list.
    member Length : int
         
    ///O(n). Returns random access list reversed.
    member Rev : unit -> RandomAccessList<'T>

    /// O(n). Returns a new random access list without the last item. If the collection is empty it throws an exception.
    member Tail : RandomAccessList<'T>

    /// O(n). Returns option random access list without the last item.
    member TryTail : RandomAccessList<'T> option

    /// O(1). Returns tuple last element and random access list without last item  
    member Uncons : 'T * RandomAccessList<'T>

    /// O(1). Returns option tuple last element and random access list without last item  
    member TryUncons : ('T * RandomAccessList<'T>) option

    /// O(log32n). Returns a new random access list that contains the given value at the index.
    member Update : int * 'T -> RandomAccessList<'T> 

    /// O(log32n). Returns option random access list that contains the given value at the index.
    member TryUpdate : int * 'T -> RandomAccessList<'T> option
            
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RandomAccessList = 
    //pattern discriminators (active pattern)
    val (|Cons|Nil|) : RandomAccessList<'T> ->  Choice<('T * RandomAccessList<'T> ),unit>
    
    /// O(1). Returns a new random access list with the element added at the end.   
    val inline cons : 'T -> RandomAccessList<'T> -> RandomAccessList<'T>

    ///O(1). Returns random access list of no elements.
    [<GeneralizableValue>]
    val empty<'T when 'T : equality> : RandomAccessList<'T>

    /// O(n). Returns a state from the supplied state and a function operating from left to right.
    val inline fold : ('State -> 'T -> 'State) -> 'State -> RandomAccessList<'T> -> 'State

    /// O(n). Returns a state from the supplied state and a function operating from right to left.
    val inline foldBack : ('T -> 'State -> 'State) -> RandomAccessList<'T> -> 'State -> 'State

    /// O(n). Returns a random access list of the supplied length using the supplied function operating on the index. 
    val init : int -> (int -> 'T) -> RandomAccessList<'T>

    /// O(1). Returns true if the random access list has no elements.
    val inline isEmpty : RandomAccessList<'T> -> bool

    /// O(1). Returns the last element in the random access list. If the random access list is empty it throws an exception.
    val inline head : RandomAccessList<'T> -> 'T

    /// O(1). Returns option last element in the random access list.
    val inline tryHead : RandomAccessList<'T> -> 'T option

    /// O(1). Returns the number of items in the random access list.
    val inline length : RandomAccessList<'T> -> int

    /// O(n). Returns a random access list whose elements are the results of applying the supplied function to each of the elements of a supplied random access list.
    val map : ('T -> 'T1) -> RandomAccessList<'T> -> RandomAccessList<'T1>

    /// O(log32n). Returns the value at the index.
    val inline nth : int -> RandomAccessList<'T> -> 'T

    /// O(log32n). Returns option value at the index. 
    val inline tryNth : int -> RandomAccessList<'T> -> 'T option
 
    /// O(n). Returns a random access list of the seq.
    val ofSeq : seq<'T> -> RandomAccessList<'T>

    ///O(n). Returns new random access list reversed.
    val inline rev : RandomAccessList<'T> -> RandomAccessList<'T>

    /// O(n). Returns a new random access list without the last item. If the collection is empty it throws an exception.
    val inline tail : RandomAccessList<'T> -> RandomAccessList<'T>

    /// O(n). Returns option random access list without the last item.
    val inline tryTail : RandomAccessList<'T> -> RandomAccessList<'T> option

    ///O(n). Views the given random access list as a sequence.
    val inline toSeq  : RandomAccessList<'T> ->  seq<'T>

    /// O(1). Returns tuple last element and random access list without last item
    val inline uncons : RandomAccessList<'T> -> 'T * RandomAccessList<'T>

    /// O(1). Returns option tuple last element and random access list without last item  
    val inline tryUncons : RandomAccessList<'T> -> ('T * RandomAccessList<'T>) option

    /// O(log32n). Returns a new random access list that contains the given value at the index. 
    val inline update : int -> 'T -> RandomAccessList<'T> -> RandomAccessList<'T>

    /// O(log32n). Returns option random access list that contains the given value at the index. 
    val inline tryUpdate : int -> 'T -> RandomAccessList<'T> -> RandomAccessList<'T> option