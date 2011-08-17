module FSharp.Monad.Iteratee.List

open System
open Operators

(* ========= Extensions ========= *)

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

(* ========= Iteratees ========= *)

let fold step seed =
  let f = List.fold step
  let rec loop acc = function
    | Empty -> Continue(loop acc)
    | Chunk [] -> Continue(loop acc)
    | Chunk xs -> Continue(loop (f acc xs))
    | EOF -> Yield(acc, EOF)
  Continue(loop seed)

let length<'a> : Iteratee<'a list, int> =
  let rec step n = function
    | Empty | Chunk [] -> Continue (step n)
    | Chunk x          -> Continue (step (n + 1))
    | EOF         as s -> Yield(n, s)
  Continue (step 0)

let rec peek =
  let rec step = function
    | Empty | Chunk []  -> peek
    | Chunk(x::xs) as s -> Yield(Some x, s)
    | s                 -> Yield(None, s)
  Continue step

let rec head =
  let rec step = function
    | Empty | Chunk [] -> head
    | Chunk(x::xs)     -> Yield(Some x, (Chunk xs))
    | EOF              -> Yield(None, EOF)
  Continue step

let rec drop n =
  let rec step = function
    | Empty | Chunk [] -> Continue step
    | Chunk x          -> drop (n - 1)
    | EOF         as s -> Yield((), s)
  if n = 0 then Yield((), Empty) else Continue step

let split (pred:char -> bool) =
  let rec step before = function
    | Empty | Chunk [] -> Continue (step before)
    | Chunk str ->
        match List.split pred str with
        | (_,[]) -> Continue (step (before @ str))
        | (str,tail) -> Yield((before @ str), Chunk tail)
    | s -> Yield(before, s)
  Continue (step [])

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
        else Yield(count, (Chunk (c'::t')))
    | _, s -> Yield(count, s)
  loop 0 str

let readLines =
  let toString chars = String(Array.ofList chars)
  let newlines = ['\r';'\n']
  let newline = ['\n']
  let isNewline c = c = '\r' || c = '\n'
  let terminators = heads newlines >>= fun n -> if n = 0 then heads newline else Yield(n, Empty)
  let rec lines acc = split isNewline >>= fun l -> terminators >>= check acc l
  and check acc l count =
    match l, count with
    | _, 0 -> Yield (Choice1Of2 (List.rev acc |> List.map toString), Chunk l)
    | [], _ -> Yield (Choice2Of2 (List.rev acc |> List.map toString), EOF)
    | l, _ -> lines (l::acc)
  lines []

(* ========= Enumerators ========= *)

//val enumerate :: 'a list -> Enumerator<'a list,'b>
let rec enumerate input = fun i ->
  match input, i with
  | [], Continue k -> Continue k
  | (x::xs), Continue k -> enumerate xs (k (Chunk [x]))
  | _, i -> i

// val enumeratePure1Chunk :: 'a list -> Enumerator<'a list,'b>
let enumeratePure1Chunk (str:'a list) = function
  | Continue k -> k (Chunk str)
  | i -> i

// val enumeratePureNChunk :: 'a list -> int -> Enumerator<'a list,'b>
let rec enumeratePureNChunk str n = fun i ->
  match str, i with
  | _::_, Continue k ->
      let (s1, s2) = List.splitAt n str
      enumeratePureNChunk s2 n (k (Chunk s1))
  | _, i -> i
