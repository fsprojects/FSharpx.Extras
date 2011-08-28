module FSharp.Monad.Iteratee.Binary
#nowarn "40"

open System
open FSharp.Collections
open Operators

(* ========= Iteratees ========= *)

let fold step seed =
  let f = ByteString.fold step
  let rec loop acc = function
    | Empty -> Continue (loop acc)
    | Chunk xs when ByteString.isEmpty xs -> Continue (loop acc)
    | Chunk xs -> Continue (loop (f acc xs))
    | EOF -> Yield(acc, EOF)
  Continue (loop seed)

let length = 
  let rec step n = function
    | Empty -> Continue (step n)
    | Chunk x when ByteString.isEmpty x -> Continue (step n)
    | Chunk x -> Continue (step (n + 1))
    | EOF as s -> Yield(n, s)
  Continue (step 0)

let rec peek =
  let rec step = function
    | Empty -> peek
    | Chunk x when ByteString.isEmpty x -> peek
    | Chunk x as s -> Yield(Some(ByteString.head x), s)
    | s -> Yield(None, s)
  Continue step

let rec head =
  let rec step = function
    | Empty -> head 
    | Chunk x when ByteString.isEmpty x -> head
    | Chunk x -> Yield(Some(ByteString.head x), Chunk(ByteString.tail x))
    | s -> Yield(None, s)
  Continue step

let rec drop n =
  let rec step n = function
    | Empty -> Continue (step n)
    | Chunk x when ByteString.isEmpty x -> Continue (step n)
    | Chunk x -> drop (n - 1)
    | s -> Yield((), s)
  if n <= 0 then Yield((), Empty) else Continue (step n)

let dropWhile pred =
  let rec step = function
    | Empty -> Continue step
    | Chunk x when ByteString.isEmpty x -> Continue step
    | Chunk x ->
        let x' = ByteString.skipWhile pred x
        in if ByteString.isEmpty x' then Continue step
           else Yield((), Chunk x')
    | s -> Yield((), s)
  Continue step

let take n =
  let rec step before n = function
    | Empty -> Continue <| step before n
    | Chunk x when ByteString.isEmpty x -> Continue <| step before n
    | Chunk x ->
        if ByteString.length x < n then
          Continue(step (ByteString.append before x) (n - (ByteString.length x)))
        else let str', extra = ByteString.splitAt n x in Yield(ByteString.append before str', Chunk extra)
    | EOF -> Yield(before, EOF)
  if n <= 0 then Yield(ByteString.empty, Empty)
  else Continue (step ByteString.empty n)

let private takeWithPredicate (pred:'a -> bool) op =
  let rec step before = function
    | Empty -> Continue (step before)
    | Chunk x when ByteString.isEmpty x -> Continue (step before)
    | Chunk x ->
        match op pred x with
        | str, extra when ByteString.isEmpty extra -> Continue (step (ByteString.append before str))
        | str, extra -> Yield(ByteString.append before str, Chunk extra)
    | EOF -> Yield(before, EOF)
  Continue (step ByteString.empty)

let takeWhile pred = takeWithPredicate pred ByteString.span
let takeUntil pred = takeWithPredicate pred ByteString.split

let heads str =
  let rec loop count str =
    if ByteString.isEmpty str then Yield(count, EOF)
    else Continue (step count str)
  and step count str = function
    | Empty -> loop count str
    | Chunk x when ByteString.isEmpty x -> loop count str
    | Chunk x when not (ByteString.isEmpty str) ->
        let c, t = ByteString.head str, ByteString.tail str
        let c', t' = ByteString.head x, ByteString.tail x
        if c = c' then step (count + 1) t (Chunk t') 
        else Yield(count, Chunk x)
    | s -> Yield(count, s)
  loop 0 str

let readLines =
  let crlf = ByteString.create "\r\n"B
  let lf = ByteString.singleton '\n'B
  let isNewline c = c = '\r'B || c = '\n'B
  let terminators = heads crlf >>= fun n -> if n = 0 then heads lf else Yield(n, Empty)
  let rec lines acc = takeUntil isNewline >>= fun bs -> terminators >>= check acc bs
  and check acc bs count =
    if count = 0 then
      Yield(Choice1Of2 (List.rev acc |> List.map ByteString.toString), Chunk bs)
    elif ByteString.isEmpty bs then
      Yield(Choice2Of2 (List.rev acc |> List.map ByteString.toString), EOF)
    else lines (bs::acc)
  lines []

(* ========= Enumerators ========= *)

// val enumerate :: ByteString -> Enumerator<ByteString,'b>
let rec enumerate str = function
  | Continue k when ByteString.isEmpty str -> Continue k
  | Continue k ->
      let x, xs = ByteString.head str, ByteString.tail str
      enumerate xs (k (Chunk (ByteString.singleton x)))
  | i -> i

// val enumeratePure1Chunk :: ByteString -> Enumerator<ByteString,'b>
let enumeratePure1Chunk (str:ByteString) = function
  | Continue k -> k (Chunk str)
  | i -> i

// val enumeratePureNChunk :: ByteString -> int -> Enumerator<ByteString,'b>
let rec enumeratePureNChunk str n = function
  | Continue k when not (ByteString.isEmpty str) ->
      let s1, s2 = ByteString.splitAt n str
      enumeratePureNChunk s2 n (k (Chunk s1))
  | i -> i

let enumStream bufferSize (stream:#System.IO.Stream) i =
  let buffer = Array.zeroCreate<byte> bufferSize
  let rec step = function Continue k -> read k | i -> i
  and read k =
    let result =
      try Choice2Of2(stream.Read(buffer, 0, bufferSize))
      with e -> Choice1Of2 e
    match result with
    | Choice1Of2 e -> throw e
    | Choice2Of2 0 -> Continue k
    | Choice2Of2 n -> step (k (Chunk(BS(buffer,0,buffer.Length))))
  step i
