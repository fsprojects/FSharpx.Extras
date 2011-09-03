module FSharp.Collections.ByteString

open System
open System.Diagnostics.Contracts

/// An alias constructor to make it easier to create ArraySegment<byte>.
let BS (x,o,l) = ArraySegment<byte>(x,o,l)
/// An active pattern for conveniently retrieving the properties of the ArraySegment<byte>.
let (|BS|) (x:ArraySegment<byte>) =
  x.Array, x.Offset, x.Count

/// Structural equality comparison for ArraySegment<byte>.
/// Use this instead of = or .Equals(other).
let equals (a:ArraySegment<byte>) (b:ArraySegment<byte>) =
  let x,o,l = a.Array, a.Offset, a.Count
  let x',o',l' = b.Array, b.Offset, b.Count
  if not (l = l') then false
  else (l = 0 && l' = 0) || (x = x' && o = o' && l = l') ||
       Array.forall2 (=) [| for i in o..(o+l-1) -> x.[i] |] [| for i' in o'..(o'+l'-1) -> x'.[i'] |]
let inline (==) a b = equals a b

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
let tail (bs:ArraySegment<byte>) = BS(bs.Array, bs.Offset+1, bs.Count-1)

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
  let x,o,l = bs.Array, bs.Offset, bs.Count
  let rec loop acc =
    if l-acc = 0 then (BS(x,o,acc), empty)
    else
      if l-(acc+1) = 0 && pred x.[o+acc] then BS(x,o,acc), empty
      elif not (pred x.[o+acc]) then BS(x,o,acc), BS(x,o+acc,l-acc)
      else loop (acc+1)
  loop 0

let split pred bs = span (not << pred) bs

let splitAt n (bs:ArraySegment<byte>) =
  Contract.Requires(n >= 0)
  let x,o,l = bs.Array, bs.Offset, bs.Count
  if n = 0 then empty, bs
  elif n >= l then bs, empty
  else BS(x,o,n), BS(x,n,l-n)

let skip n bs = splitAt n bs |> snd
let skipWhile pred bs = span pred bs |> snd
let skipUntil pred bs = split pred bs |> snd
let take n bs = splitAt n bs |> fst 
let takeWhile pred bs = span pred bs |> fst
let takeUntil pred bs = split pred bs |> fst 
