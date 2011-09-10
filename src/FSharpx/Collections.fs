namespace FSharpx

open System
open System.Collections
open System.Collections.Generic
#if NET40
open System.Diagnostics.Contracts
#endif

/// Functions to create tuples
module Tuples =
  /// Creates a pair
  let inline t2 a b = a,b
  /// Creates a 3-tuple
  let inline t3 a b c = a,b,c
  /// Creates a 4-tuple
  let inline t4 a b c d = a,b,c,d
  /// Creates a 5-tuple
  let inline t5 a b c d e = a,b,c,d,e
  /// Creates a 6-tuple
  let inline t6 a b c d e f = a,b,c,d,e,f

module Seq =
    /// <summary>
    /// Adds an index to a sequence
    /// </summary>
    /// <param name="a"></param>
    let index a = Seq.zip (Seq.initInfinite id) a

    /// <summary>
    /// Returns the first element (with its index) for which the given function returns true.
    /// Return None if no such element exists.
    /// </summary>
    /// <param name="pred">Predicate</param>
    /// <param name="l">Sequence</param>
    let tryFindWithIndex pred l =
        l |> index |> Seq.tryFind (fun (_,v) -> pred v)

module List =
  /// Curried cons
  let inline cons hd tl = hd::tl

  let inline singleton x = [x]

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
type BS =
  struct
    val Array: byte[]
    val Offset: int
    val Count: int
    new (array: byte[]) = { Array = array; Offset = 0; Count = array.Length }
    new (array: byte[], offset: int, count: int) = { Array = array; Offset = offset; Count = count }
    static member Compare (a:BS, b:BS) =
      let x,o,l = a.Array, a.Offset, a.Count
      let x',o',l' = b.Array, b.Offset, b.Count
      if x = x' && o = o' && l = l' then 0
      elif x = x' then
        if o = o' then if l < l' then -1 else 1
        else if o < o' then -1 else 1 
      else let left, right = x.[o..(o+l-1)], x'.[o'..(o'+l'-1)] in
           if left = right then 0 elif left < right then -1 else 1
    override x.Equals(other) = 
      match other with
      | :? BS as other' -> BS.Compare(x, other') = 0
      | _ -> false
    override x.GetHashCode() = hash x
    interface System.IComparable with
      member x.CompareTo(other) =
        match other with
        | :? BS as other' -> BS.Compare(x, other')
        | _ -> invalidArg "other" "Cannot compare a value of another type."
  end
  
module ByteString =

  /// An active pattern for conveniently retrieving the properties of a BS.
  let (|BS|) (x:BS) = x.Array, x.Offset, x.Count
  
  let empty = BS()
  let singleton c = BS(Array.create 1 c, 0, 1)
  let create arr = BS(arr, 0, arr.Length)
  let ofArraySegment (segment:ArraySegment<byte>) = BS(segment.Array, segment.Offset, segment.Count)
  let ofSeq s = let arr = Array.ofSeq s in BS(arr, 0, arr.Length)
  let ofList l = BS(Array.ofList l, 0, l.Length)
  let ofString (s:string) = s.ToCharArray() |> Array.map byte |> create
  let toArray (bs:BS) = bs.Array.[bs.Offset..(bs.Count - 1)]
  let toSeq (bs:BS) =
    seq { for i in bs.Offset..(bs.Offset + bs.Count - 1) do yield bs.Array.[i] }
  let toList (bs:BS) =
    [ for i in bs.Offset..(bs.Offset + bs.Count - 1) -> bs.Array.[i] ]
  let toString (bs:BS) =
    System.Text.Encoding.ASCII.GetString(bs.Array, bs.Offset, bs.Count)
  let isEmpty (bs:BS) = 
    #if NET40
    Contract.Requires(bs.Count >= 0)
    #endif
    bs.Count <= 0
  let length (bs:BS) = 
    #if NET40
    Contract.Requires(bs.Count >= 0)
    #endif
    bs.Count
  let index (bs:BS) pos =
    #if NET40
    Contract.Requires(bs.Offset + pos <= bs.Count)
    #endif
    bs.Array.[bs.Offset + pos]
  let head (bs:BS) =
    if bs.Count <= 0 then
      failwith "Cannot take the head of an empty byte string."
    else bs.Array.[bs.Offset]
  let tail (bs:BS) =
    #if NET40
    Contract.Requires(bs.Count >= 1)
    #endif
    if bs.Count = 1 then empty
    else BS(bs.Array, bs.Offset+1, bs.Count-1)
  
  /// cons uses Buffer.SetByte and Buffer.BlockCopy for efficient array operations.
  /// Please note that a new array is created and both the head and tail are copied in,
  /// disregarding any additional bytes in the original tail array.
  let cons hd (bs:BS) =
    let x,o,l = bs.Array, bs.Offset, bs.Count in
    if l = 0 then singleton hd
    else let buffer = Array.zeroCreate<byte> (l + 1)
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
         let buffer = Array.zeroCreate<byte> (l + l')
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

  let span pred (bs:BS) =
    if isEmpty bs then empty, empty
    else
      let x,o,l = bs.Array, bs.Offset, bs.Count
      let rec loop acc =
        if l = acc + 1 && pred x.[o+acc] then bs, empty
        elif not (pred x.[o+acc]) then BS(x,o,acc), BS(x,o+acc,l-acc)
        else loop (acc+1)
      loop 0
  
  let split pred bs = span (not << pred) bs
  
  let splitAt n (bs:BS) =
    #if NET40
    Contract.Requires(n >= 0)
    #endif
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameValueCollection =
    open System.Collections.Specialized
    open System.Linq

    /// <summary>
    /// Returns a new <see cref="NameValueCollection"/> with the concatenation of two <see cref="NameValueCollection"/>s
    /// </summary>
    /// <param name="a"></param>
    /// <param name="b"></param>
    let concat a b = 
      let x = NameValueCollection()
      x.Add a
      x.Add b
      x

    /// <summary>
    /// In-place add of a key-value pair to a <see cref="NameValueCollection"/>
    /// </summary>
    /// <param name="x"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    let inline addInPlace (x: NameValueCollection) (a,b) = x.Add(a,b)

    /// Adds an element to a copy of an existing NameValueCollection
    let add name value (x: NameValueCollection) =
      let r = NameValueCollection x
      r.Add(name,value)
      r

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a sequence of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    let toSeq (a: NameValueCollection) =
      a.AllKeys
      |> Seq.collect (fun k -> a.GetValues k |> Seq.map (fun v -> k,v))

    /// <summary>
    /// Returns a <see cref="NameValueCollection"/> as a list of key-value pairs.
    /// Note that keys may be duplicated.
    /// </summary>
    /// <param name="a"></param>
    let inline toList a = toSeq a |> Seq.toList

    /// <summary>
    /// Creates a <see cref="NameValueCollection"/> from a list of key-value pairs
    /// </summary>
    /// <param name="l"></param>
    let fromSeq l =
      let x = NameValueCollection()
      Seq.iter (addInPlace x) l
      x

    let toLookup a =
      let s = toSeq a
      s.ToLookup(fst, snd)