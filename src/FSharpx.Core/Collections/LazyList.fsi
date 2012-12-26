// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack/LazyList.fsi

// (c) Microsoft Corporation 2005-2009. 

namespace FSharpx.Collections

open System.Collections.Generic

/// LazyLists are possibly-infinite, cached sequences.  See also IEnumerable/Seq for
/// uncached sequences. LazyLists normally involve delayed computations without 
/// side-effects.  The results of these computations are cached and evaluations will be 
/// performed only once for each element of the lazy list.  In contrast, for sequences 
/// (IEnumerable) recomputation happens each time an enumerator is created and the sequence 
/// traversed.
///
/// LazyLists can represent cached, potentially-infinite computations.  Because they are 
/// cached they may cause memory leaks if some active code or data structure maintains a 
/// live reference to the head of an infinite or very large lazy list while iterating it, 
/// or if a reference is maintained after the list is no longer required.
///
/// Lazy lists may be matched using the LazyList.Cons and LazyList.Nil active patterns. 
/// These may force the computation of elements of the list.

[<Sealed>]
type LazyList<'T> =
    interface IEnumerable<'T>
    interface System.Collections.IEnumerable
    
    ///O(1). Test if a list is empty.  Forces the evaluation of
    /// the first element of the stream if it is not already evaluated.
    member IsEmpty : bool

    ///O(1). Return the first element of the list.  Forces the evaluation of
    /// the first cell of the list if it is not already evaluated.
    member Head : 'T

    ///O(n). Return the length of the list
    member Length : unit -> int

    ///O(1). Return option the first element of the list.  Forces the evaluation of
    /// the first cell of the list if it is not already evaluated.
    member TryHead : 'T option

    ///O(1). Return the list corresponding to the remaining items in the sequence.  
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    member Tail : LazyList<'T>

    ///O(1). Return option the list corresponding to the remaining items in the sequence.  
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    member TryTail : LazyList<'T> option

    ///O(1). Returns tuple of head element and tail of the list.
    member Uncons : 'T * LazyList<'T>

    ///O(1). Returns option tuple of head element and tail of the list.
    member TryUncons : ('T * LazyList<'T>) option

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LazyList =

    ///O(1). Test if a list is empty.  Forces the evaluation of
    /// the first element of the stream if it is not already evaluated.
    val isEmpty: LazyList<'T> -> bool

    ///O(1). Return the first element of the list.  Forces the evaluation of
    /// the first cell of the list if it is not already evaluated.
    val head       : LazyList<'T> -> 'T

    ///O(1). Return option the first element of the list.  Forces the evaluation of
    /// the first cell of the list if it is not already evaluated.
    val tryHead       : LazyList<'T> -> 'T option

    ///O(1). Return the list corresponding to the remaining items in the sequence.  
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    val tail       : LazyList<'T> -> LazyList<'T>

    ///O(1). Return option the list corresponding to the remaining items in the sequence.  
    /// Forces the evaluation of the first cell of the list if it is not already evaluated.
    val tryTail       : LazyList<'T> -> LazyList<'T> option

    ///O(1). Returns tuple of head element and tail of the list.
    val uncons      : LazyList<'T> -> 'T * LazyList<'T>

    ///O(1). Returns option tuple of head element and tail of the list.
    val tryUncons      : LazyList<'T> -> ('T * LazyList<'T>) option

    ///O(n), where n is count. Return the list which on consumption will consist of at most 'n' elements of 
    /// the input list.  
    val take     : count:int -> source:LazyList<'T> -> LazyList<'T>

    ///O(n), where n is count. Return the list which on consumption will consist of at most 'n' elements of 
    /// the input list.  
    val tryTake     : count:int -> source:LazyList<'T> -> LazyList<'T> option

    ///O(n), where n is count. Return the list which on consumption will skip the first 'n' elements of 
    /// the input list.  
    val skip     : count:int -> source:LazyList<'T> -> LazyList<'T>

    ///O(n), where n is count. Return option the list which skips the first 'n' elements of 
    /// the input list.  
    val trySkip     : count:int -> source:LazyList<'T> -> LazyList<'T> option

    ///O(n). Behaves like a combination of map and fold; 
    /// it applies a function to each element of a list, 
    /// passing an accumulating parameter from left to right, 
    /// and returning a final value of this accumulator together with the new list.
    val mapAccum     : f:('T1 -> 'T2 -> 'T1 * 'T3) -> s:'T1 -> l:LazyList<'T2> -> 'T1 * LazyList<'T3>

    ///O(n), worst case. Apply the given function to successive elements of the list, returning the first
    /// result where function returns <c>Some(x)</c> for some x. If the function never returns
    /// true, 'None' is returned.
    val tryFind    : predicate:('T -> bool) -> source:LazyList<'T> -> 'T option

    ///O(n), worst case. Return the first element for which the given function returns <c>true</c>.
    /// Raise <c>KeyNotFoundException</c> if no such element exists.
    val find     : predicate:('T -> bool) -> source:LazyList<'T> -> 'T 

    ///O(1). Evaluates to the list that contains no items
    [<GeneralizableValue>]
    val empty<'T>    : LazyList<'T>

    ///O(n). Return the length of the list
    val length: list:LazyList<'T> -> int

    ///O(1). Return a new list which contains the given item followed by the
    /// given list.
    val cons     : 'T -> LazyList<'T>               -> LazyList<'T>

    ///O(1). Return a new list which on consumption contains the given item 
    /// followed by the list returned by the given computation.  The 
    val consDelayed    : 'T -> (unit -> LazyList<'T>)     -> LazyList<'T>

    ///O(1). Return the list which on consumption will consist of an infinite sequence of 
    /// the given item
    val repeat   : 'T -> LazyList<'T>

    ///O(1). Return a list that is in effect the list returned by the given computation.
    /// The given computation is not executed until the first element on the list is
    /// consumed.
    val delayed  : (unit -> LazyList<'T>)           -> LazyList<'T>

    ///O(1). Return a list that contains the elements returned by the given computation.
    /// The given computation is not executed until the first element on the list is
    /// consumed.  The given argument is passed to the computation.  Subsequent elements
    /// in the list are generated by again applying the residual 'b to the computation.
    val unfold   : ('State -> ('T * 'State) option) -> 'State -> LazyList<'T>

    ///O(1). Return the list which contains on demand the elements of the first list followed
    /// by the elements of the second list
    val append   : LazyList<'T> -> source:LazyList<'T> -> LazyList<'T>

    ///O(1). Return the list which contains on demand the pair of elements of the first and second list
    val zip  : LazyList<'T1> -> LazyList<'T2> -> LazyList<'T1 * 'T2>

    ///O(1). Return the list which contains on demand the list of elements of the list of lazy lists.
    val concat   : LazyList< LazyList<'T>> -> LazyList<'T>

    ///O(1). Return a new collection which on consumption will consist of only the elements of the collection
    /// for which the given predicate returns "true"
    val filter   : predicate:('T -> bool) -> source:LazyList<'T> -> LazyList<'T>

    ///O(n). Apply the given function to each element of the collection. 
    val iter: action:('T -> unit) -> list:LazyList<'T>-> unit

    ///O(1). Return a new list consisting of the results of applying the given accumulating function
    /// to successive elements of the list
    val scan    : folder:('State -> 'T -> 'State) -> 'State -> source:LazyList<'T> -> LazyList<'State>  

    ///O(1). Build a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection.
    val map      : mapping:('T -> 'U) -> source:LazyList<'T> -> LazyList<'U>

    ///O(1). Build a new collection whose elements are the results of applying the given function
    /// to the corresponding elements of the two collections pairwise.
    val map2     : mapping:('T1 -> 'T2 -> 'U) -> LazyList<'T1> -> LazyList<'T2> -> LazyList<'U>

    ///O(1). Build a collection from the given array. This function will eagerly evaluate all of the 
    /// list (and thus may not terminate). 
    val ofArray : 'T array -> LazyList<'T>

    ///O(n). Build an array from the given collection
    val toArray : LazyList<'T> -> 'T array

    ///O(1). Build a collection from the given list. This function will eagerly evaluate all of the 
    /// list (and thus may not terminate). 
    val ofList  : list<'T> -> LazyList<'T>

    ///O(n). Build a non-lazy list from the given collection. This function will eagerly evaluate all of the 
    /// list (and thus may not terminate). 
    val toList  : LazyList<'T> -> list<'T>

    ///O(n). Return a view of the collection as an enumerable object
    val toSeq: LazyList<'T> -> seq<'T>

    ///O(1). Build a new collection from the given enumerable object
    val ofSeq: seq<'T> -> LazyList<'T>

    //--------------------------------------------------------------------------
    // Lazy list active patterns

    val (|Cons|Nil|) : LazyList<'T> -> Choice<('T * LazyList<'T>),unit>

