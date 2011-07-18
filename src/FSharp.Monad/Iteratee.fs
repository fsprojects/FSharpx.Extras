module FSharp.Monad.Iteratee

open System.IO
open Monoid

type Stream<'a> =
  | Chunk of 'a list
  | EOF

type ChunkMonoid<'a>() =
  interface IMonoid<Stream<'a>> with
    member this.mempty() = Chunk []
    member this.mappend(a,b) = 
      match a with
      | Chunk []
      | EOF -> match b with EOF -> EOF | _ -> b
      | Chunk xs ->
          match b with
          | Chunk []
          | EOF -> a
          | Chunk ys -> Chunk (xs @ ys)

MonoidAssociations.Add(new ChunkMonoid<_>())

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
  | Yield(x, Chunk []) -> f x
  | Yield(x, extra) ->
      match f x with
      | Continue k -> k extra
      | Error e -> Error e
      | Yield(acc',_) -> Yield(acc', extra)

type IterateeBuilder() =
  member this.Return(x) = Yield(x, mempty())
  member this.ReturnFrom(m:Iteratee<_,_>) = m
  member this.Bind(m, k) = bind m k
  member this.Zero() = Yield((), mempty())
  member this.Combine(comp1, comp2) = bind comp1 (fun () -> comp2)
  member this.Delay(f) = bind (Yield((), mempty())) f
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
  | Choice2Of2 x -> x

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
  | str, n, Continue k ->
      let (s1, s2) = (Seq.take n str |> List.ofSeq, Seq.skip n str |> List.ofSeq)
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
    | Choice2Of2 n' -> loop (k (Chunk (buffer |> List.ofArray))) // Not happy about this. Need to investigate switching to Seq.
  loop i

