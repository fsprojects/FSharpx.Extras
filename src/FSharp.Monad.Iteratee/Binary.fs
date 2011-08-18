module FSharp.Monad.Iteratee.Binary
#nowarn "40"

open System
open FSharp.Collections
open Operators

(* ========= Iteratees ========= *)

let fold step seed =
  let f = ByteString.fold step
  let rec loop acc = function
    | Empty -> continueI (loop acc)
    | Chunk xs when ByteString.isEmpty xs -> continueI (loop acc)
    | Chunk xs -> continueI (loop (f acc xs))
    | EOF -> yieldI acc EOF
  continueI (loop seed)

let length = 
  let rec step n = function
    | Empty -> continueI (step n)
    | Chunk x when ByteString.isEmpty x -> continueI (step n)
    | Chunk x -> continueI (step (n + 1))
    | EOF as s -> yieldI n s
  continueI (step 0)

let rec peek =
  let rec step = function
    | Empty -> peek
    | Chunk x when ByteString.isEmpty x -> peek
    | Chunk x as s -> yieldI (Some(ByteString.head x)) s
    | s -> yieldI None s
  continueI step

let rec head =
  let rec step = function
    | Empty -> head 
    | Chunk x when ByteString.isEmpty x -> head
    | Chunk x -> yieldI (Some(ByteString.head x)) (Chunk(ByteString.tail x))
    | s -> yieldI None s
  continueI step

let rec drop n =
  let rec step = function
    | Empty -> continueI step
    | Chunk x when ByteString.isEmpty x -> continueI step
    | Chunk x -> drop (n - 1)
    | s -> yieldI () s
  if n <= 0 then yieldI () Empty else continueI step

let dropWhile pred =
  let rec step = function
    | Empty -> continueI step
    | Chunk x when ByteString.isEmpty x -> continueI step
    | Chunk x ->
        let x' = ByteString.ofSeq <| Seq.skipWhile pred x
        in if ByteString.isEmpty x' then continueI step else yieldI () (Chunk x')
    | s -> yieldI () s
  continueI step

let split pred =
  let rec step before = function
    | Empty -> continueI (step before)
    | Chunk x when ByteString.isEmpty x -> continueI (step before)
    | Chunk str ->
        match ByteString.split pred str with
        | (_,x) when ByteString.isEmpty x -> continueI (step (ByteString.append before str))
        | (str,tail) -> yieldI (ByteString.append before str) (Chunk tail)
    | s -> yieldI before s
  continueI (step ByteString.empty)

let heads str =
  let rec loop count str =
    if ByteString.isEmpty str then yieldI count EOF
    else continueI (step count str)
  and step count str = function
    | Empty -> loop count str
    | Chunk x when ByteString.isEmpty x -> loop count str
    | Chunk x' when not (ByteString.isEmpty str) ->
        let c, t = ByteString.head str, ByteString.tail str
        let c', t' = ByteString.head x', ByteString.tail x'
        if c = c' then step (count + 1) t (Chunk t') 
        else yieldI count (Chunk x')
    | s -> yieldI count s
  loop 0 str

let readLines =
  let crlf = ByteString.create "\r\n"B
  let lf = ByteString.singleton '\n'B
  let isNewline c = c = '\r'B || c = '\n'B
  let terminators = heads crlf >>= fun n -> if n = 0 then heads lf else yieldI n Empty
  let rec lines acc = split isNewline >>= fun bs -> terminators >>= check acc bs
  and check acc bs count =
    if count = 0 then yieldI (Choice1Of2 (List.rev acc |> List.map (ByteString.toString))) (Chunk bs)
    elif ByteString.isEmpty bs then yieldI (Choice2Of2 (List.rev acc |> List.map (ByteString.toString))) EOF
    else lines (bs::acc)
  lines []

(* ========= Enumerators ========= *)

//val enumerate :: ByteString -> Enumerator<ByteString,'b>
let rec enumerate input i =
  match runIter i with 
  | Continue k when ByteString.isEmpty input -> continueI k
  | Continue k ->
      let x, xs = ByteString.head input, ByteString.tail input
      enumerate xs (k (Chunk (ByteString.singleton x)))
  | _ -> i

// val enumeratePure1Chunk :: ByteString -> Enumerator<ByteString,'b>
let enumeratePure1Chunk (str:ByteString) i =
  match runIter i with 
  | Continue k -> k (Chunk str)
  | _ -> i

// val enumeratePureNChunk :: ByteString -> int -> Enumerator<ByteString,'b>
let rec enumeratePureNChunk str n i =
  match runIter i with
  | Continue k when not (ByteString.isEmpty str) ->
      let (s1, s2) = ByteString.splitAt n str
      enumeratePureNChunk s2 n (k (Chunk s1))
  | _ -> i

let enumStream bufferSize (stream:#System.IO.Stream) i =
  let buffer = Array.zeroCreate<byte> bufferSize
  let rec step i =
    match runIter i with
    | Continue k -> read k
    | _ -> i
  and read k =
    let result =
      try Choice2Of2(stream.Read(buffer, 0, bufferSize))
      with e -> Choice1Of2 e
    match result with
    | Choice1Of2 e -> throw e
    | Choice2Of2 0 -> continueI k
    | Choice2Of2 n -> step (k (Chunk (BS(buffer,0,buffer.Length))))
  step i
