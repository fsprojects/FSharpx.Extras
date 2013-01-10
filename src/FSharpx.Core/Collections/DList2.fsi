namespace FSharpx.Collections

/// The DList is an implementation of John Hughes' append list.
/// It offers standare list structure with O(1) append.
/// See http://dl.acm.org/citation.cfm?id=8475 for more information.
/// An example can be found at http://stackoverflow.com/questions/5324623/functional-o1-append-and-on-iteration-from-first-element-list-data-structure/5327209#5327209
/// List-like structure naming semantics: http://jackfoxy.com/semantics-and-list-like-data-structures

[<Class>]
//[<Sealed>]
type DList<'T> =

    interface System.Collections.Generic.IEnumerable<'T>
    interface System.Collections.IEnumerable
   
    ///O(1). Returns the count of elememts.
    member Length : int

    ///O(1). Returns a new DList with the element added to the front.
    member Cons : 'T ->  DList<'T>

    ///O(log n). Returns the first element.
    member Head : 'T 

    ///O(log n). Returns option first element
    member TryHead : 'T  option

    ///O(1). Returns true if the DList has no elements.
    member IsEmpty : bool

    ///O(1). Returns a new DList with the element added to the end.
    member Snoc : 'T ->  DList<'T>

    ///O(log n). Returns a new DList of the elements trailing the first element.
    member Tail : DList<'T>
           
    ///O(log n). Returns option DList of the elements trailing the first element.
    member TryTail : DList<'T> option

    ///O(log n). Returns the first element and tail.
    member Uncons : 'T * DList<'T> 
 
    ///O((log n). Returns option first element and tail.
    member TryUncons : ('T * DList<'T>) option

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DList =
    //pattern discriminators (active pattern)
    val (|Cons|Nil|) : DList<'T> -> Choice<('T * DList<'T>),unit>

    ///O(1). Returns a new DList of two lists.
    val append : DList<'T> -> DList<'T> -> DList<'T>

    ///O(1). Returns a new DList with the element added to the beginning.
    val cons : 'T -> DList<'T> -> DList<'T>

    ///O(1). Returns DList of no elements.
    [<GeneralizableValue>]
    val empty<'T> : DList<'T>

    ///O(n). Fold walks the DList using constant stack space. Implementation is from Norman Ramsey.
    /// See http://stackoverflow.com/questions/5324623/functional-o1-append-and-on-iteration-from-first-element-list-data-structure/5334068#5334068
    val foldBack : ('T -> 'State -> 'State) -> DList<'T> -> 'State -> 'State

    val fold : ('State -> 'T -> 'State) -> 'State -> DList<'T> -> 'State

    ///O(log n). Returns the first element.
    val inline head : DList<'T> -> 'T

    ///O(log n). Returns option first element.
    val inline tryHead : DList<'T> -> 'T option

    ///O(1). Returns true if the DList has no elements.
    val inline isEmpty : DList<'T> -> bool

    ///O(1). Returns the count of elememts.
    val inline length : DList<'T> -> int

    ///O(1). Returns DList of one elements.
    val singleton : 'T -> DList<'T>

    ///O(1). Returns a new DList with the element added to the end.
    val inline snoc : 'T -> DList<'T> -> DList<'T>

    ///O(log n). Returns a new DList of the elements trailing the first element.
    val inline tail : DList<'T> -> DList<'T>

    ///O(log n). Returns option DList of the elements trailing the first element.
    val inline tryTail : DList<'T> -> DList<'T> option

    ///O(log n). Returns the first element and tail.
    val inline uncons : DList<'T> -> 'T * DList<'T>

    ///O(log n). Returns option first element and tail.
    val inline tryUncons : DList<'T> -> ('T * DList<'T>) option

    ///O(n). Returns a DList of the seq.
    val ofSeq : seq<'T> -> DList<'T>

    ///O(n). Returns a list of the DList elements.
    val inline toList : DList<'T> -> list<'T>

    ///O(n). Returns a seq of the DList elements.
    val inline toSeq  : DList<'T> ->  seq<'T>