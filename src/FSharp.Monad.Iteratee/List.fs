module FSharp.Monad.Iteratee.List
#nowarn "40"

open System
open Operators

(* ========= Extensions ========= *)

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

  let skipWhile pred l = span pred l |> snd
  let skipUntil pred l = split pred l |> snd
  let takeWhile pred l = span pred l |> fst
  let takeUntil pred l = split pred l |> fst

(* ========= Iteratees ========= *)

let fold step seed =
  let f = List.fold step
  let rec loop acc = function
    | Empty -> Continue (loop acc)
    | Chunk [] -> Continue (loop acc)
    | Chunk xs -> Continue (loop (f acc xs))
    | EOF -> Yield(acc, EOF)
  Continue (loop seed)

let length<'a> : Iteratee<'a list, int> =
  let rec step n = function
    | Empty | Chunk [] -> Continue (step n)
    | Chunk x          -> Continue (step (n + 1))
    | EOF         as s -> Yield(n, s)
  in Continue (step 0)

let peek<'a> : Iteratee<'a list, 'a option> =
  let rec inner =
    let rec step = function
      | Empty | Chunk ([]:'a list) -> inner
      | Chunk(x::xs) as s -> Yield(Some x, s)
      | s -> Yield(None, s)
    Continue step
  in inner

let head<'a> : Iteratee<'a list, 'a option> =
  let rec inner =
    let rec step = function
      | Empty | Chunk ([]:'a list) -> inner
      | Chunk(x::xs) -> Yield(Some x, Chunk xs)
      | EOF -> Yield(None, EOF)
    Continue step
  in inner

let rec drop n =
  let rec step = function
    | Empty | Chunk [] -> Continue step
    | Chunk x          -> drop (n - 1)
    | EOF         as s -> Yield((), s)
  in if n <= 0 then Yield((), Empty) else Continue step

let dropWhile pred =
  let rec step = function
    | Empty | Chunk [] -> Continue step
    | Chunk x ->
        match List.skipWhile pred x with
        | [] -> Continue step
        | x' -> Yield((), Chunk x')
    | EOF as s -> Yield((), s)
  in Continue step

let take n =
  let rec step before n = function
    | Empty | Chunk [] -> Continue <| step before n
    | Chunk str ->
        if str.Length < n then
          Continue <| step (before @ str) (n - str.Length)
        else let str', extra = List.splitAt n str in Yield(before @ str', Chunk extra)
    | EOF -> Yield(before, EOF)
  in if n <= 0 then Yield([], Empty) else Continue (step [] n)

let private takeWithPredicate (pred:'a -> bool) listOp =
  let rec step before = function
    | Empty | Chunk [] -> Continue (step before)
    | Chunk str ->
        match listOp pred str with
        | str', [] -> Continue (step (before @ str'))
        | str', extra -> Yield(before @ str', Chunk extra)
    | EOF -> Yield(before, EOF)
  in Continue (step [])

let takeWhile pred = takeWithPredicate pred List.span
let takeUntil pred = takeWithPredicate pred List.split

let heads str =
  let rec loop count str =
    match count, str with
    | (count, []) -> Yield(count, EOF)
    | (count, str) -> Continue (step count str)
  and step count str s =
    match str, s with
    | str, Empty -> loop count str
    | str, (Chunk []) -> loop count str
    | c::t, (Chunk (c'::t')) ->
        if c = c' then step (count + 1) t (Chunk t') 
        else Yield(count, Chunk (c'::t'))
    | _, s -> Yield(count, s)
  loop 0 str

let readLines =
  let toString chars = String(Array.ofList chars)
  let newlines = ['\r';'\n']
  let newline = ['\n']
  let isNewline c = c = '\r' || c = '\n'
  let terminators = heads newlines >>= fun n -> if n = 0 then heads newline else Yield(n, Empty)
  let rec lines acc = takeUntil isNewline >>= fun l -> terminators >>= check acc l
  and check acc l count =
    match l, count with
    | _, 0 -> Yield(Choice1Of2 (List.rev acc |> List.map toString), Chunk l)
    | [], _ -> Yield(Choice2Of2 (List.rev acc |> List.map toString), EOF)
    | l, _ -> lines (l::acc)
  lines []

(* ========= Enumerators ========= *)

//val enumerate :: 'a list -> Enumerator<'a list,'b>
let rec enumerate str i = 
  match str, i with
  | [], Continue k -> Continue k
  | (x::xs), Continue k -> enumerate xs (k (Chunk [x]))
  | _ -> i

// val enumeratePure1Chunk :: 'a list -> Enumerator<'a list,'b>
let enumeratePure1Chunk (str:'a list) = function
  | Continue k -> k (Chunk str)
  | i -> i

// val enumeratePureNChunk :: 'a list -> int -> Enumerator<'a list,'b>
let rec enumeratePureNChunk str n i =
  match str, i with
  | _::_, Continue k ->
      let s1, s2 = List.splitAt n str
      enumeratePureNChunk s2 n (k (Chunk s1))
  | _ -> i
