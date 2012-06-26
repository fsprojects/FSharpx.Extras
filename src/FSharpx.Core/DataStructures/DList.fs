namespace FSharpx.DataStructures

open FSharpx
open System.Collections
open System.Collections.Generic

/// The DList is an implementation of John Hughes' append list.
/// See http://dl.acm.org/citation.cfm?id=8475 for more information.
/// This implementation adds an additional parameter to allow a more
/// efficient calculation of the list length.
/// Note that an alternate form would represent the DList as:
/// type DList<'a> = DList of ('a list -> 'a list)
/// An example can be found at http://stackoverflow.com/questions/5324623/functional-o1-append-and-on-iteration-from-first-element-list-data-structure/5327209#5327209
type DList<'a> =
    | Nil
    | Unit of 'a
    | Join of DList<'a> * DList<'a> * int (* the total length of the DList *)
    with

    static member op_Equality(left, right) =
        match left with
        | Nil -> match right with Nil -> true | _ -> false
        | Unit x -> match right with Unit y -> x = y | _ -> false
        | Join(x,y,l) ->
            match right with
            | Join(x',y',l') -> l = l' && x = x' && y = y' // TODO: || iterate each and compare the values.
            | _ -> false 

    static member op_Nil() = Nil

    member x.Length =
        match x with
        | Nil -> 0 
        | Unit _ -> 1
        | Join(_,_,l) -> l

    static member op_Cons(hd, tl) =
        match tl with
        | Nil -> Unit hd
        | _ -> Join(Unit hd, tl, tl.Length + 1)

    static member op_Append(left, right) =
        match left with
        | Nil -> right
        | _ -> match right with
               | Nil -> left
               | _ -> Join(left, right, left.Length + right.Length)

    member x.IsEmpty = match x with Nil -> true | _ -> false

    member x.Head =
        match x with
        | Unit x' -> x'
        | Join(x',y,_) -> x'.Head
        | _ -> failwith "DList.head: empty list"

    member x.Tail =
        let rec step (xs:DList<'a>) (acc:DList<'a>) : DList<'a> =
            match xs with
            | Nil -> acc
            | Unit _ -> acc
            | Join(x,y,_) -> step x (DList<_>.op_Append(y, acc))
        if x.IsEmpty then Nil else step x Nil

    interface IEnumerable<'a> with
        member x.GetEnumerator() =
            let enumerable = seq {
                match x with
                | Nil -> () 
                | Unit x -> yield x
                | Join(x,y,_) ->
                    yield! x :> seq<'a>
                    yield! y :> seq<'a> }
            enumerable.GetEnumerator()

        member x.GetEnumerator() =
            let enumerable = seq {
                match x with
                | Nil -> () 
                | Unit x -> yield x
                | Join(x,y,_) ->
                    yield! x :> seq<'a>
                    yield! y :> seq<'a> }
            enumerable.GetEnumerator() :> IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DList =
    let empty<'a> : DList<'a> = Nil

    let isEmpty (l:DList<_>) = l.IsEmpty

    let length (l:DList<_>) = l.Length

    let singleton x = Unit x

    let ofSeq s = Seq.fold (fun xs x ->
        match xs with 
        | Nil -> Unit x
        | Unit _ -> Join(xs, Unit x, 2)
        | Join(_,_,l) -> Join(xs, Unit x, l+1)) Nil s

    let toSeq (l:DList<_>) = l :> seq<_>

    let cons hd tl = DList<_>.op_Cons(hd, tl)

    let append right left = DList<_>.op_Append(left, right)

    let head (l:DList<_>) = l.Head

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

    let toList l = fold (flip List.cons) [] l

    let toArray l = l |> toList |> Array.ofList
