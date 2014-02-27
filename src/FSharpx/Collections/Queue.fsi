namespace FSharpx.Collections

/// Queue is an ordered linear data structure where elements are added at the end (right) 
/// and inspected and removed at the beginning (left). Ordering is by insertion history. 
/// The qualities of the Queue structure make elements first in, first out (fifo).
/// "head" inspects the first or left-most element in the structure, while "conj" 
/// inserts an element at the end, or right of the structure.
/// Purely functional (immutable) Queue based on Okasaki's batched queue. 
/// Value and function naming standard based on consistent List-like naming: http://jackfoxy.com/semantics-and-list-like-data-structures
/// Original F# implementation http://lepensemoi.free.fr/index.php/2009/12/10/batched-queue
[<Class>]
type Queue<'T> =

    interface System.Collections.Generic.IEnumerable<'T>
    interface System.Collections.IEnumerable

    ///O(1). Returns a new queue with the element added to the end. (Enqueue)
    member Conj : 'T ->  Queue<'T>

    ///O(1). Returns the first element. (Peek)
    member Head : 'T 

    ///O(1). Returns option first element
    member TryHead : 'T  option

    ///O(1). Returns true if the queue has no elements.
    member IsEmpty : bool

    ///O(1). Returns the count of elememts.
    member Length : int

    ///O(n). Returns queue reversed.
    member Rev : unit -> Queue<'T>

    ///O(1) amortized, O(n) worst-case. Returns a new queue of the elements trailing the first element. (Dequeue)
    member Tail : Queue<'T>
           
    ///O(1) amortized, O(n) worst-case. Returns option queue of the elements trailing the first element.
    member TryTail : Queue<'T> option

    ///O(1) amortized, O(n) worst-case. Returns the first element and tail.
    member Uncons : 'T * Queue<'T> 
 
    ///O(1) amortized, O(n) worst-case. Returns option first element and tail.
    member TryUncons : ('T * Queue<'T>) option

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Queue =
    //pattern discriminators (active pattern)
    val (|Cons|Nil|) : Queue<'T> -> Choice<('T * Queue<'T>),unit>

    ///O(1). Returns a new queue with the element added to the end. (enqueue)
    val inline conj : 'T -> Queue<'T> -> Queue<'T>

    ///O(1). Returns queue of no elements.
    [<GeneralizableValue>]
    val empty<'T> : Queue<'T> 

    ///O(n). Applies a function to each element of the queue, threading an accumulator argument through the computation, left to right.
    val fold : ('State -> 'T -> 'State) -> 'State -> Queue<'T> -> 'State

    ///O(n). Applies a function to each element of the queue, threading an accumulator argument through the computation, right to left.
    val foldBack : ('T -> 'State -> 'State) -> Queue<'T> -> 'State -> 'State 

    ///O(1). Returns the first element. (peek)
    val inline head : Queue<'T> -> 'T

    ///O(1). Returns option first element.
    val inline tryHead : Queue<'T> -> 'T option

    ///O(1). Returns true if the queue has no elements.
    val inline isEmpty : Queue<'T> -> bool

    ///O(1). Returns the count of elememts.
    val inline length : Queue<'T> -> int

    ///O(1). Returns a queue of the list
    val ofList : list<'T> -> Queue<'T>

    ///O(n). Returns a queue of the seq.
    val ofSeq : seq<'T> -> Queue<'T>

    ///O(n). Returns queue reversed.
    val inline rev : Queue<'T> -> Queue<'T>

    ///O(1) amortized, O(n) worst-case. Returns a new queue of the elements trailing the first element. (dequeue)
    val inline tail : Queue<'T> -> Queue<'T>

    ///O(1) amortized, O(n) worst-case. Returns option queue of the elements trailing the first element
    val inline tryTail : Queue<'T> -> Queue<'T> option

    ///O(n). Views the given queue as a sequence.
    val inline toSeq  : Queue<'T> ->  seq<'T>

    ///O(1) amortized, O(n) worst-case. Returns the first element and tail.
    val inline uncons : Queue<'T> -> 'T * Queue<'T>

    ///O(1) amortized, O(n) worst-case. Returns option first element and tail.
    val inline tryUncons : Queue<'T> -> ('T * Queue<'T>) option