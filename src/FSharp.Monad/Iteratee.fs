module FSharp.Monad.Iteratee

open System.IO

module List =
  let split pred l =
    let rec loop l cont =
      match l with
      | [] -> ([],[])
      | x::[] when not (pred x) -> (cont l, [])
      | x::xs when pred x -> (cont [], l)
      | x::xs when not (pred x) -> loop xs (fun rest -> cont (x::rest))
      | _ -> failwith "List.split: Unrecognized pattern"
    loop l id

  let splitAt n l =
    let pred i = i >= n
    let rec loop i l cont =
      match l with
      | [] -> ([],[])
      | x::[] when not (pred i) -> (cont l, [])
      | x::xs when pred i -> (cont [], l)
      | x::xs when not (pred i) -> loop (i+1) xs (fun rest -> cont (x::rest))
      | _ -> failwith "List.splitAt: Unrecognized pattern"
    loop 0 l id

module ByteString =
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
open ByteString

type Stream<'a> =
  | Chunk of 'a
  | Empty
  | EOF

type Iteratee<'el,'acc> =
  | Continue of (Stream<'el> -> Iteratee<'el,'acc>)
  | Yield of 'acc * Stream<'el>
  | Error of exn

type Enumerator<'el,'acc> = Iteratee<'el,'acc> -> Iteratee<'el,'acc>

type Enumeratee<'elo,'eli,'acc> = Iteratee<'eli,'acc> -> Iteratee<'elo, Iteratee<'eli,'acc>>

let rec bind m f =
  match m with
  | Continue k -> Continue(fun s -> bind (k s) f)
  | Error e -> Error e
  | Yield(x, Empty) -> f x
  | Yield(x, extra) ->
      match f x with
      | Continue k -> k extra
      | Error e -> Error e
      | Yield(acc',_) -> Yield(acc', extra)

let combine comp1 comp2 =
  let binder () = comp2
  bind comp1 binder

type IterateeBuilder() =
  member this.Return(x) = Yield(x, Empty)
  member this.ReturnFrom(m:Iteratee<_,_>) = m
  member this.Bind(m, k) = bind m k
  member this.Zero() = Yield((), Empty)
  member this.Combine(comp1, comp2) = combine comp1 comp2
  member this.Delay(f) = bind (Yield((), Empty)) f
let iteratee = IterateeBuilder()

module Operators =
  open FSharp.Monad.Operators

  let inline returnM x = returnM iteratee x
  let inline (>>=) m f = bindM iteratee m f
  let inline (<*>) f m = applyM iteratee iteratee f m
  let inline lift f m = liftM iteratee f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM iteratee m (fun _ -> f)

let rec enumEOF = function
  | Yield(x,_) -> Yield(x,EOF)
  | Error e -> Error e
  | Continue k ->
      match k EOF with
      | Continue _ -> failwith "enumEOF: divergent iteratee"
      | i -> enumEOF i

let run i =
  match enumEOF i with
  | Error e -> Choice1Of2 e
  | Yield(x,_) -> Choice2Of2 x
  | Continue _ -> failwith "run: divergent iteratee"

let run_ i =
  match run i with
  | Choice1Of2 e -> raise e
  | x -> x

//val enumerate :: 'a list -> Enumerator<'a,'b,'c>
let rec enumerate input i =
  match input, i with
  | []     , Continue k -> Continue k
  | (x::xs), Continue k -> enumerate xs (k (Chunk [x]))
  | _ , i -> i

// val enumeratePure1Chunk :: 'a -> Enumerator<'a,'b,'c>
let enumeratePure1Chunk str i =
  match str, i with
  | str, Continue k -> k (Chunk str)
  | _  , i -> i

// val enumeratePureNChunk :: 'a list -> int -> Enumerator<'a,'b,'c>
let rec enumeratePureNChunk str n i =
  match str, n, i with
  | _::_, n, Continue k ->
      let (s1, s2) = List.splitAt n str
      enumeratePureNChunk s2 n (k (Chunk s1))
  | _  , _, i -> i

let enumStream bufferSize (stream:Stream) i =
  let buffer = Array.zeroCreate<byte> bufferSize
  let rec loop i =
    match i with
    | Continue k -> read k
    | x -> x
  and read k =
    let result =
      try Choice2Of2(stream.Read(buffer, 0, bufferSize))
      with e -> Choice1Of2 e
    match result with
    | Choice1Of2 e  -> Error e
    | Choice2Of2 0  -> Continue k
    | Choice2Of2 n' -> loop (k (Chunk (BS(buffer,0,buffer.Length))))
  loop i
