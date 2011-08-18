namespace FSharp.Collections

open System.Collections
open System.Collections.Generic
open System.Diagnostics.Contracts

type JoinList<'a> =
  | EmptyJoinList
  | Unit of 'a
  | Join of JoinList<'a> * JoinList<'a> * int (* the total length of the JoinList *)
  with
  interface IEnumerable<'a> with
    member this.GetEnumerator() =
      let enumerable = seq {
        match this with
        | EmptyJoinList -> () 
        | Unit x -> yield x
        | Join(x,y,_) ->
            yield! x :> seq<'a>
            yield! y :> seq<'a> }
      enumerable.GetEnumerator()
    member this.GetEnumerator() =
      let enumerable = seq {
        match this with
        | EmptyJoinList -> () 
        | Unit x -> yield x
        | Join(x,y,_) ->
            yield! x :> seq<'a>
            yield! y :> seq<'a> }
      enumerable.GetEnumerator() :> IEnumerator

module JoinList =
  let empty<'a> : JoinList<'a> = EmptyJoinList
  let isEmptyJoinList l = match l with EmptyJoinList -> true | _ -> false
  let length l =
    match l with
    | EmptyJoinList -> 0 
    | Unit _ -> 1
    | Join(_,_,l) -> l
  let singleton x = Unit x
  let ofSeq s = Seq.fold (fun xs x ->
    match xs with 
    | EmptyJoinList -> Unit x
    | Unit _ -> Join(xs, Unit x, 2)
    | Join(_,_,l) -> Join(xs, Unit x, l+1)) EmptyJoinList s
  let toSeq (l:JoinList<_>) = l :> seq<_>
  let toList (l:JoinList<_>) = List.ofSeq l   // NOTE: There is likely a better conversion to the List type.
  let toArray (l:JoinList<_>) = Array.ofSeq l // NOTE: There is likely a better conversion to the Array type.
  let rec equal left right =
    match left with
    | EmptyJoinList -> match right with EmptyJoinList -> true | _ -> false
    | Unit x -> match right with Unit y -> x = y | _ -> false
    | Join(x,y,l) ->
      match right with
      | Join(x',y',l') -> l = l' && equal x x' && equal y y' // TODO: || iterate each and compare the values.
      | _ -> false 
  let cons hd tl =
    match tl with
    | EmptyJoinList -> Unit hd
    | _ -> Join(Unit hd, tl, length tl + 1)
  let append left right =
    match left with
    | EmptyJoinList -> right
    | _ -> match right with
           | EmptyJoinList -> left
           | _ -> Join(left, right, length left + length right)
  let rec head l =
    match l with
    | Unit x -> x
    | Join(x,y,_) -> head x
    | _ -> failwith "JoinList.head: empty list"
  let tail (l:JoinList<'a>) : JoinList<'a> =
    let rec step (xs:JoinList<'a>) (acc:JoinList<'a>) : JoinList<'a> =
      match xs with
      | EmptyJoinList -> acc
      | Unit _ -> acc
      | Join(x,y,_) -> step x (append y acc)
    if isEmptyJoinList l then EmptyJoinList else step l EmptyJoinList
        
type JoinList<'a> with
  static member op_Equality(left, right) = JoinList.equal left right
  static member op_Nil() = JoinList.empty
  static member op_Cons(hd, tl) = JoinList.cons hd tl
  static member op_Append(left, right) = JoinList.append left right
