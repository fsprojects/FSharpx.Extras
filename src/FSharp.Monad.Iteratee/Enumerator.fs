module FSharp.Monad.Enumerator

open Iteratee

/// An enumerator generates a stream of data and feeds an iteratee, returning a new iteratee.
type Enumerator<'el,'acc> = Iteratee<'el,'acc> -> Iteratee<'el,'acc>

// TODO: EnumeratorBuilder

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

module List =
  open FSharp.Monad

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

module IO =
  open ByteString

  let enumStream bufferSize (stream:System.IO.Stream) i =
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
  