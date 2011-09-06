namespace FSharpx

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics.Contracts

module List =

  let span pred l =
    let rec loop l cont =
      match l with
      | [] -> ([],[])
      | x::[] when pred x -> (cont l, [])
      | x::xs when not (pred x) -> (cont [], l)
      | x::xs when pred x -> loop xs (fun rest -> cont (x::rest))
      | _ -> failwith "Unrecognized pattern"
    loop l id

  let split pred l = span (not << pred) l

  let skipWhile pred l = span pred l |> snd
  let skipUntil pred l = split pred l |> snd
  let takeWhile pred l = span pred l |> fst
  let takeUntil pred l = split pred l |> fst
  
  let splitAt n l =
    let pred i = i >= n
    let rec loop i l cont =
      match l with
      | [] -> ([],[])
      | x::[] when not (pred i) -> (cont l, [])
      | x::xs when pred i -> (cont [], l)
      | x::xs when not (pred i) -> loop (i+1) xs (fun rest -> cont (x::rest))
      | _ -> failwith "Unrecognized pattern"
    loop 0 l id

  let skip n l = splitAt n l |> snd
  let take n l = splitAt n l |> fst

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

/// An ArraySegment with structural comparison and equality.
[<CustomEquality; CustomComparison>]
[<SerializableAttribute>]
type ArraySegment<'a when 'a : comparison> =
  struct
    val Array: 'a[]
    val Offset: int
    val Count: int
    new (array: 'a[]) = { Array = array; Offset = 0; Count = array.Length }
    new (array: 'a[], offset: int, count: int) = { Array = array; Offset = offset; Count = count }
    static member Compare (a:ArraySegment<'a>, b:ArraySegment<'a>) =
      let x,o,l = a.Array, a.Offset, a.Count
      let x',o',l' = b.Array, b.Offset, b.Count
      if x = x' && o = o' && l = l' then 0
      elif x = x' then
        if o = o' then if l < l' then -1 else 1
        else if o < o' then -1 else 1 
      else let foldr res b b' =
              if res <> 0 then res
              else if b = b' then 0
                   elif b < b' then -1
                   else 1
           let left = [| for i in o..(o+l-1) -> x.[i] |]
           let right = [| for i' in o'..(o'+l'-1) -> x'.[i'] |]
           Array.fold2 foldr 0 left right
    override x.Equals(other) = 
      match other with
      | :? ArraySegment<'a> as other' -> ArraySegment.Compare(x, other') = 0
      | _ -> false
    override x.GetHashCode() = hash x
    interface System.IComparable with
      member x.CompareTo(other) =
        match other with
        | :? ArraySegment<'a> as other' -> ArraySegment.Compare(x, other')
        | _ -> invalidArg "other" "Cannot compare a value of another type."
  end
  
module ByteString =
  open System.Diagnostics.Contracts

  /// An alias constructor to make it easier to create ArraySegment<byte>.
  let BS (x,o,l) = ArraySegment<byte>(x,o,l)
  /// An active pattern for conveniently retrieving the properties of the ArraySegment<byte>.
  let (|BS|) (x:ArraySegment<byte>) =
    x.Array, x.Offset, x.Count
  
  let empty = ArraySegment<byte>()
  let singleton c = BS(Array.create 1 c, 0, 1)
  let create bs = BS(bs, 0, bs.Length)
  let ofSeq s = let arr = Array.ofSeq s in BS(arr, 0, arr.Length)
  let ofList l = BS(Array.ofList l, 0, l.Length)
  let ofString (s:string) = s.ToCharArray() |> Array.map byte |> create
  let toSeq (bs:ArraySegment<byte>) =
    seq { for i in bs.Offset..(bs.Offset + bs.Count - 1) do yield bs.Array.[i] }
  let toList (bs:ArraySegment<byte>) =
    [ for i in bs.Offset..(bs.Offset + bs.Count - 1) -> bs.Array.[i] ]
  let toString (bs:ArraySegment<byte>) =
    System.Text.Encoding.ASCII.GetString(bs.Array, bs.Offset, bs.Count)
  let isEmpty (bs:ArraySegment<byte>) = Contract.Requires(bs.Count >= 0); bs.Count <= 0
  let length (bs:ArraySegment<byte>) = Contract.Requires(bs.Count >= 0); bs.Count
  let index (bs:ArraySegment<byte>) pos =
    Contract.Requires(bs.Offset + pos <= bs.Count)
    bs.Array.[bs.Offset + pos]
  let head (bs:ArraySegment<byte>) =
    if bs.Count <= 0 then
      failwith "Cannot take the head of an empty byte string."
    else bs.Array.[bs.Offset]
  let tail (bs:ArraySegment<byte>) =
    Contract.Requires(bs.Count >= 1)
    if bs.Count = 1 then empty
    else BS(bs.Array, bs.Offset+1, bs.Count-1)
  
  /// cons uses Buffer.SetByte and Buffer.BlockCopy for efficient array operations.
  /// Please note that a new array is created and both the head and tail are copied in,
  /// disregarding any additional bytes in the original tail array.
  let cons hd (bs:ArraySegment<byte>) =
    let x,o,l = bs.Array, bs.Offset, bs.Count in
    if l = 0 then singleton hd
    else let buffer = Array.init (l + 1) byte
         Buffer.SetByte(buffer,0,hd)
         Buffer.BlockCopy(x,o,buffer,1,l)
         BS(buffer,0,l+1)
  
  /// append uses Buffer.BlockCopy for efficient array operations.
  /// Please note that a new array is created and both arrays are copied in,
  /// disregarding any additional bytes in the original, underlying arrays.
  let append a b = 
    if isEmpty a then b
    elif isEmpty b then a
    else let x,o,l = a.Array, a.Offset, a.Count
         let x',o',l' = b.Array, b.Offset, b.Count
         let buffer = Array.init (l + l') byte
         Buffer.BlockCopy(x,o,buffer,0,l)
         Buffer.BlockCopy(x',o',buffer,l,l')
         BS(buffer,0,l+l')
  
  let fold f seed bs =
    let rec loop bs acc =
      if isEmpty bs then acc 
      else
        let hd, tl = head bs, tail bs
        loop tl (f acc hd)
    loop bs seed

  let span pred (bs:ArraySegment<byte>) =
    if isEmpty bs then empty, empty
    else
      let x,o,l = bs.Array, bs.Offset, bs.Count
      let rec loop acc =
        if l = acc + 1 && pred x.[o+acc] then bs, empty
        elif not (pred x.[o+acc]) then BS(x,o,acc), BS(x,o+acc,l-acc)
        else loop (acc+1)
      loop 0
  
  let split pred bs = span (not << pred) bs
  
  let splitAt n (bs:ArraySegment<byte>) =
    Contract.Requires(n >= 0)
    if isEmpty bs then empty, empty
    elif n = 0 then empty, bs
    elif n >= bs.Count then bs, empty
    else let x,o,l = bs.Array, bs.Offset, bs.Count in BS(x,o,n), BS(x,o+n,l-n)
  
  let skip n bs = splitAt n bs |> snd
  let skipWhile pred bs = span pred bs |> snd
  let skipUntil pred bs = split pred bs |> snd
  let take n bs = splitAt n bs |> fst 
  let takeWhile pred bs = span pred bs |> fst
  let takeUntil pred bs = split pred bs |> fst 
