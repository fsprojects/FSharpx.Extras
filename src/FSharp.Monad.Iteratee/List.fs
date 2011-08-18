module FSharp.Monad.Iteratee.List
#nowarn "40"

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
    | Empty -> continueI (loop acc)
    | Chunk [] -> continueI (loop acc)
    | Chunk xs -> continueI (loop (f acc xs))
    | EOF -> yieldI acc EOF
  continueI (loop seed)

let length<'a> : Iteratee<'a list, int> =
  let rec step n = function
    | Empty | Chunk [] -> continueI (step n)
    | Chunk x          -> continueI (step (n + 1))
    | EOF         as s -> yieldI n s
  continueI (step 0)

let peek<'a> : Iteratee<'a list, 'a option> =
  let rec inner =
    let rec step = function
      | Empty | Chunk ([]:'a list) -> inner
      | Chunk(x::xs) as s -> yieldI (Some x) s
      | s -> yieldI (None: 'a option) s
    continueI step
  inner

let head<'a> : Iteratee<'a list, 'a option> =
  let rec inner =
    let rec step = function
      | Empty | Chunk ([]:'a list) -> inner
      | Chunk(x::xs) -> yieldI (Some x) (Chunk xs)
      | EOF -> yieldI None (EOF:Stream<'a list>)
    continueI step
  inner

let rec drop n =
  let rec step = function
    | Empty | Chunk [] -> continueI step
    | Chunk x          -> drop (n - 1)
    | EOF         as s -> yieldI () s
  if n <= 0 then yieldI () Empty else continueI step

let dropWhile pred =
  let rec step = function
    | Empty | Chunk [] -> continueI step
    | Chunk x ->
        match List.ofSeq <| Seq.skipWhile pred x with
        | [] -> continueI step
        | x' -> yieldI () (Chunk x')
    | EOF as s -> yieldI () s
  continueI step

let split (pred:char -> bool) =
  let rec step before = function
    | Empty | Chunk [] -> continueI (step before)
    | Chunk str ->
        match List.split pred str with
        | (_,[]) -> continueI (step (before @ str))
        | (str,tail) -> yieldI (before @ str) (Chunk tail)
    | s -> yieldI before s
  continueI (step [])

let heads str =
  let rec loop count str =
    match count, str with
    | (count, []) -> yieldI count EOF
    | (count, str) -> continueI (step count str)
  and step count str s =
    match str, s with
    | str, Empty -> loop count str
    | str, (Chunk []) -> loop count str
    | c::t, (Chunk (c'::t')) ->
        if c = c' then step (count + 1) t (Chunk t') 
        else yieldI count (Chunk (c'::t'))
    | _, s -> yieldI count s
  loop 0 str

let readLines =
  let toString chars = String(Array.ofList chars)
  let newlines = ['\r';'\n']
  let newline = ['\n']
  let isNewline c = c = '\r' || c = '\n'
  let terminators = heads newlines >>= fun n -> if n = 0 then heads newline else yieldI n Empty
  let rec lines acc = split isNewline >>= fun l -> terminators >>= check acc l
  and check acc l count =
    match l, count with
    | _, 0 -> yieldI (Choice1Of2 (List.rev acc |> List.map toString)) (Chunk l)
    | [], _ -> yieldI (Choice2Of2 (List.rev acc |> List.map toString)) EOF
    | l, _ -> lines (l::acc)
  lines []

(* ========= Enumerators ========= *)

//val enumerate :: 'a list -> Enumerator<'a list,'b>
let rec enumerate input i = 
  match input, runIter i with
  | [], Continue k -> continueI k
  | (x::xs), Continue k -> enumerate xs (k (Chunk [x]))
  | _ -> i

// val enumeratePure1Chunk :: 'a list -> Enumerator<'a list,'b>
let enumeratePure1Chunk (str:'a list) i =
  match runIter i with
  | Continue k -> k (Chunk str)
  | _ -> i

// val enumeratePureNChunk :: 'a list -> int -> Enumerator<'a list,'b>
let rec enumeratePureNChunk str n i =
  match str, runIter i with
  | _::_, Continue k ->
      let (s1, s2) = List.splitAt n str
      enumeratePureNChunk s2 n (k (Chunk s1))
  | _ -> i
