namespace FSharp.Collections

open System
open System.Diagnostics.Contracts

/// ByteString is a functional ArraySegment<byte> that also implements IEnumerable<byte>.
/// NOTE: This type should really be moved into a set of extension types missing from F#.
type ByteString = BS of byte array * int * int // TODO: Switch to the JoinList for more efficient cons and append.
  with
  interface System.Collections.Generic.IEnumerable<byte> with
    member this.GetEnumerator() =
      let (BS(a,o,l)) = this
      let inner = seq { for i in o..(l-1) do yield a.[i] }
      inner.GetEnumerator()
    member this.GetEnumerator() =
      let (BS(a,o,l)) = this
      let inner = seq { for i in o..(l-1) do yield a.[i] }
      inner.GetEnumerator() :> System.Collections.IEnumerator

  static member op_Equality (BS(x,o,l), BS(x',o',l')) =
    if not (l = l') then false
    else (l = 0 && l' = 0) || (x = x' && o = o') // TODO: Add byte by byte comparison

  /// Implements the [] operator, which is not available on anything but the List<_> type.
  static member op_Nil = BS(Array.empty,0,0)

  /// Implements the :: operator, which is not available on anything but the List<_> type.
  /// op_Cons uses Buffer.SetByte and Buffer.BlockCopy for efficient array operations.
  /// Please note that a new array is created and both the head and tail are copied in,
  /// disregarding any additional bytes in the original tail array.
  static member op_Cons (hd, BS(x,o,l)) =
    if l = 0 then ByteString.singleton hd
    else let buffer = Array.init (l + 1) byte
         Buffer.SetByte(buffer,0,hd)
         Buffer.BlockCopy(x,o,buffer,1,l)
         BS(buffer,0,l+1)

  /// Implements the @ operator, which is not available on anything but the List<_> type.
  /// op_Append uses Buffer.BlockCopy for efficient array operations.
  /// Please note that a new array is created and both arrays are copied in,
  /// disregarding any additional bytes in the original, underlying arrays.
  static member op_Append (a, b) =
    if ByteString.isEmpty a then b
    elif ByteString.isEmpty b then a
    else let (BS(x,o,l)) = a
         let (BS(x',o',l')) = b
         let buffer = Array.init (l + l') byte
         Buffer.BlockCopy(x,o,buffer,0,l)
         Buffer.BlockCopy(x',o',buffer,l,l')
         BS(buffer,0,l+l')

  static member empty = ByteString.op_Nil
  static member singleton c = BS(Array.create 1 c, 0, 1)
  static member create bs = BS(bs, 0, bs.Length)
  static member ofSegment (segment:ArraySegment<byte>) = BS(segment.Array, segment.Offset, segment.Count)
  static member ofSeq s = let arr = Array.ofSeq s in BS(arr, 0, arr.Length)
  static member ofList l = BS(Array.ofList l, 0, l.Length)
  static member ofString (s:string) = s.ToCharArray() |> Array.map byte |> ByteString.create
  static member toList (BS(x,o,l)) = let len = l-1 in [ for i in o..len -> x.[i] ]
  static member toString (BS(x,o,l)) = let len = l-1 in System.Text.Encoding.ASCII.GetString(x,o,l)
  static member isEmpty (BS(_,_,l)) = Contract.Requires(l >= 0); l <= 0
  static member length (BS(_,_,l)) = Contract.Requires(l >= 0); l
  static member index (BS(x,o,l)) pos = Contract.Requires(o + pos <= l); x.[o + pos]
  static member head (BS(x,o,l)) = if l <= 0 then failwith "" else x.[o]
  static member tail (BS(x,o,l)) = BS(x,o+1,l-1)
  static member cons hd tl = ByteString.op_Cons(hd, tl)
  static member append a b = ByteString.op_Append(a, b)
  static member fold f seed bs =
    let rec loop bs acc =
      if ByteString.isEmpty bs then acc 
      else
        let hd, tl = ByteString.head bs, ByteString.tail bs
        loop tl (f acc hd)
    loop bs seed
  
  static member span pred (BS(x,o,l) as bs) =
    let rec loop acc =
      if l-acc = 0 then (BS(x,o,acc), ByteString.empty)
      else
        if l-(acc+1) = 0 && pred x.[o+acc] then BS(x,o,acc), ByteString.empty
        elif not (pred x.[o+acc]) then BS(x,o,acc), BS(x,o+acc,l-acc)
        else loop (acc+1)
    loop 0
 
  static member split pred bs = ByteString.span (not << pred) bs

  static member splitAt n (BS(x,o,l) as bs) =
    Contract.Requires(n >= 0)
    if n = 0 then ByteString.empty, bs
    elif n >= l then bs, ByteString.empty
    else BS(x,o,n), BS(x,n,l-n)

  static member skip n bs = ByteString.splitAt n bs |> snd
  static member skipWhile pred bs = ByteString.span pred bs |> snd
  static member skipUntil pred bs = ByteString.split pred bs |> snd
  static member take n bs = ByteString.splitAt n bs |> fst 
  static member takeWhile pred bs = ByteString.span pred bs |> fst
  static member takeUntil pred bs = ByteString.split pred bs |> fst 
