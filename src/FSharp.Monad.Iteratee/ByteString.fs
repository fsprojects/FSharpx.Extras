module FSharp.Monad.ByteString

open System
open System.Diagnostics

// Consider switching to ArraySegment, which is mutable.
type ByteString = BS of byte array * int * int
  with
  static member op_Equality (BS(x,o,l), BS(x',o',l')) =
    if not (l = l') then false
    else (l = 0 && l' = 0) || (x = x' && o = o') // TODO: Add byte by byte comparison

  static member op_Nil = BS(Array.empty,0,0)

  static member op_Cons (hd, BS(x,o,l)) =
    let buffer = Array.zeroCreate<byte> (l + 1)
    Buffer.SetByte(buffer,0,hd)
    Buffer.BlockCopy(x,o,buffer,1,l)
    BS(buffer,0,l+1)

  static member op_Append (BS(x,o,l), BS(x',o',l')) =
    let buffer = Array.zeroCreate<byte> (l + l')
    Buffer.BlockCopy(x,o,buffer,0,l)
    Buffer.BlockCopy(x',o',buffer,l,l')
    BS(buffer,0,l+l')

  interface System.Collections.Generic.IEnumerable<byte> with
    member x.GetEnumerator() =
      let (BS(a,o,l)) = x
      let inner = seq { for i in o..l do yield a.[i] }
      inner.GetEnumerator()
    member x.GetEnumerator() =
      let (BS(a,o,l)) = x
      let inner = seq { for i in o..l do yield a.[i] }
      inner.GetEnumerator() :> System.Collections.IEnumerator

let empty = ByteString.op_Nil
let singleton c = BS(Array.create 1 c, 0, 1)
let ofList l = BS(Array.ofList l, 0, l.Length)
let toList (BS(x,o,l)) = [ for i in o..l -> x.[i] ]
let isEmpty (BS(_,_,l)) = Debug.Assert(l >= 0); l <= 0
let length (BS(_,_,l)) = Debug.Assert(l >= 0); l
let head (BS(x,o,l)) = if l <= 0 then failwith "" else x.[o]
let tail (BS(x,o,l)) = BS(x,o+1,l-1)
let cons hd tl = ByteString.op_Cons(hd, tl)
let append a b = ByteString.op_Append(a, b)

let split pred l =
  let rec loop l cont =
    if isEmpty l then (empty, empty)
    elif isEmpty (tail l) && not (pred (head l)) then (cont l, empty)
    elif pred (head l) then (cont empty, l)
    elif not (pred (head l)) then loop (tail l) (fun rest -> cont (cons (head l) rest))
    else failwith "ByteString.split: Unrecognized pattern"
  loop l id

let splitAt n l =
  let pred i = i >= n
  let rec loop i l cont =
    if isEmpty l then (empty, empty)
    elif isEmpty (tail l) && not (pred i) then (cont l, empty)
    elif pred i then (cont empty, l)
    elif not (pred i) then loop (i+1) (tail l) (fun rest -> cont (cons (head l) rest))
    else failwith "ByteString.splitAt: Unrecognized pattern"
  loop 0 l id
