namespace FSharpx.Collections.Experimental

open FSharpx
open System.Collections
open System.Collections.Generic

/// The DList is an implementation of John Hughes' append list.
/// See http://dl.acm.org/citation.cfm?id=8475 for more information.
/// This implementation adds an additional parameter to allow a more
/// efficient calculation of the list length.
/// Note that an alternate form would represent the DList as:
/// type DList<'T> = DList of ('T list -> 'T list)
/// An example can be found at http://stackoverflow.com/questions/5324623/functional-o1-append-and-on-iteration-from-first-element-list-data-structure/5327209#5327209
type DList<'T> =
    | Nil
    | Unit of 'T
    | Join of DList<'T> * DList<'T> * int (* the total length of the DList *)

    static member op_Equality(left, right) =
        match left with
        | Nil -> match right with Nil -> true | _ -> false
        | Unit x -> match right with Unit y -> x = y | _ -> false
        | Join(x,y,l) ->
            match right with
            | Join(x',y',l') -> l = l' && x = x' && y = y' // TODO: || iterate each and compare the values.
            | _ -> false 

    static member op_Nil() = Nil

    ///O(1). Returns the count of elememts.
    member x.Length =
        match x with
        | Nil -> 0 
        | Unit _ -> 1
        | Join(_,_,l) -> l

    ///O(1). Returns true if the DList has no elements.
    member x.IsEmpty = match x with Nil -> true | _ -> false

    ///O(log n). Returns the first element.
    member x.Head =
        match x with
        | Unit x' -> x'
        | Join(x',y,_) -> x'.Head
        | _ -> failwith "DList.head: empty list"

    ///O(1). Returns a new DList with the element added to the end.
    member x.snoc (a:'T) =
        //DList<_>.op_Append(x, Unit a)
        match x with
        | Nil ->  Unit a
        | _ -> match  Unit a with
               | Nil -> x
               | _ -> Join(x,  (Unit a), x.Length +  (Unit a).Length)

    ///O(log n). Returns a new DList of the elements trailing the first element.
    member x.Tail =
        let rec step (xs:DList<'T>) (acc:DList<'T>) : DList<'T> =
            match xs with
            | Nil -> acc
            | Unit _ -> acc
            | Join(x,y,_) -> 
                step x (match y with
                        | Nil -> acc
                        | _ -> match acc with
                               | Nil -> y
                               | _ -> Join(y, acc, y.Length + acc.Length))

        if x.IsEmpty then Nil else step x Nil

    interface IEnumerable<'T> with
        member x.GetEnumerator() =
            let enumerable = seq {
                match x with
                | Nil -> () 
                | Unit x -> yield x
                | Join(x,y,_) ->
                    yield! x :> seq<'T>
                    yield! y :> seq<'T> }
            enumerable.GetEnumerator()

        member x.GetEnumerator() =
            let enumerable = seq {
                match x with
                | Nil -> () 
                | Unit x -> yield x
                | Join(x,y,_) ->
                    yield! x :> seq<'T>
                    yield! y :> seq<'T> }
            enumerable.GetEnumerator() :> IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DList =
    ///O(1). Returns DList of no elements.
    let empty<'T> : DList<'T> = Nil

    ///O(1). Returns true if the DList has no elements.
    let isEmpty (l:DList<_>) = l.IsEmpty

    ///O(1). Returns the count of elememts.
    let length (l:DList<_>) = l.Length

    ///O(1). Returns DList of one elements.
    let singleton x = Unit x

    ///O(n). Returns a DList of the seq.
    let ofSeq s = Seq.fold (fun xs x ->
        match xs with 
        | Nil -> Unit x
        | Unit _ -> Join(xs, Unit x, 2)
        | Join(_,_,l) -> Join(xs, Unit x, l+1)) Nil s

    ///O(n). Returns a seq of the DList elements.
    let toSeq (l:DList<_>) = l :> _ seq

    ///O(1). Returns a new DList with the element added to the beginning.
    let cons hd tl = 
        match tl with
        | Nil -> Unit hd
        | _ -> Join(Unit hd, tl, tl.Length + 1)

    ///O(1). Returns a new DList of two lists.
    let append left right =
        match left with
        | Nil -> right
        | _ -> match right with
               | Nil -> left
               | _ -> Join(left, right, left.Length + right.Length)

    ///O(log n). Returns the first element.
    let head (l:DList<_>) = l.Head

    ///O(1). Returns a new DList with the element added to the end.
    let snoc (l:DList<_>) x = l.snoc x

    ///O(log n). Returns a new DList of the elements trailing the first element.
    let tail (l:DList<_>) = l.Tail

    /// Fold walks the DList using constant stack space. Implementation is from Norman Ramsey.
    /// See http://stackoverflow.com/questions/5324623/functional-o1-append-and-on-iteration-from-first-element-list-data-structure/5334068#5334068
    let fold f seed l =
        let rec walk lefts l xs =
            match l with
            | Nil         -> finish lefts xs
            | Unit x      -> finish lefts <| f xs x
            | Join(x,y,_) -> walk (x::lefts) y xs
        and finish lefts xs =
            match lefts with
            | []    -> xs
            | t::ts -> walk ts t xs
        in walk [] l seed

    let toList l = fold (flip FSharpx.Collections.List.cons) [] l

    let toArray l = l |> toList |> Array.ofList
