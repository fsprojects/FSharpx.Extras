module FSharp.Monad.Tests.IterateeTest

open System
open System.Linq
open FSharp.Monad.Iteratee
open FSharp.Monad.Iteratee.Operators
open NUnit.Framework
open FsUnit

module List =
  let split p l =
    let n = List.tryFindIndex p l
    match n with
    | Some x -> (Seq.take x l |> List.ofSeq, Seq.skip x l |> List.ofSeq)
    | _ -> (l,[])

let runTest i =
  match run i with
  | Choice1Of2 e -> raise e
  | Choice2Of2 x -> x

let counter<'a> : Iteratee<'a,int> =
  let rec step n = function
    | Chunk [] as s -> Continue (step n)
    | Chunk x  as s -> Continue (step (n + 1))
    | EOF      as s -> Yield(n, s)
  Continue (step 0)

[<Test>]
let ``test counter should calculate the length of the list without modification``() =
  let actual = enumerate [1;2;3] counter
  runTest actual |> should equal 3

let rec peek =
  let rec step = function
    | Chunk []     as s -> peek
    | Chunk(x::xs) as s -> Yield(Some x, s)
    | s                 -> Yield(None, s)
  Continue step

let testPeek = [|
  [| box ([]:char list); box None |]
  [| box ['c']; box (Some 'c') |]
  [| box ['c';'h';'a';'r']; box (Some 'c') |]
|]
[<Test>]
[<TestCaseSource("testPeek")>]
let ``test peek should return the value without removing it from the stream``(input:char list, expected:char option) =
  let actual = enumerate input peek
  runTest actual |> should equal expected

let rec head =
  let rec step = function
    | Chunk []     as s -> head
    | Chunk(x::xs) as s -> Yield(Some x, (Chunk xs))
    | EOF               -> Yield(None, EOF)
  Continue step

let testHead = [|
  [| box ([]:char list); box None |]
  [| box ['c']; box (Some 'c') |]
  [| box ['c';'h';'a';'r']; box (Some 'c') |]
|]
[<Test>]
[<TestCaseSource("testHead")>]
let ``test head should return the value and remove it from the stream``(input:char list, expected:char option) =
  let actual = enumerate input head
  runTest actual |> should equal expected

let rec drop n =
  let rec step = function
    | Chunk [] as s -> Continue step
    | Chunk x  as s -> drop (n - 1)
    | EOF      as s -> Yield((), s)
  if n = 0 then Yield((), Chunk []) else Continue step

let split (pred:char -> bool) =
  let ieContM k = Continue k
  let rec step before = function
    | Chunk [] -> ieContM (step before)
    | Chunk str ->
        match List.split pred str with
        | (_,[]) -> ieContM (step (before @ str))
        | (str,tail) -> Yield((before @ str), Chunk tail)
    | s     -> Yield(before, s)
  Continue (step [])

[<Test>]
let ``test split should correctly split the input``() =
  let actual = enumeratePure1Chunk (List.ofSeq "abcde") (split ((=) 'c'))
  runTest actual |> should equal ['a';'b']

let heads str =
  let rec loop count str =
    match count, str with
    | (count, []) -> Yield(count, EOF)
    | (count, str) -> Continue (step count str)
  and step count str s =
    let str = List.ofSeq str
    match count, str, s with
    | count, str, (Chunk []) -> loop count str
    | count, c::t, (Chunk (c'::t')) ->
        if c = c' then step (count + 1) t (Chunk t') 
        else Yield(count, (Chunk (c'::t')))
    | count, _, s -> Yield(count, s)
  loop 0 str

[<Test>]
let ``test heads should count the number of characters in a set of headers``() =
  let actual = enumeratePure1Chunk (List.ofSeq "abd") (heads (List.ofSeq "abc"))
  runTest actual |> should equal 2

let readLines =
  let toString chars = String(Array.ofList chars)
  let terminators = heads ['\r';'\n'] >>= (fun n -> if n = 0 then heads ['\n'] else Yield(n, Chunk []))
  let rec lines acc =
    split (fun c -> c = '\r' || c = '\n') >>= fun l -> terminators >>= check acc l
  and check acc l count =
    match acc, l, count with
    | acc,  l, 0 -> Yield (Choice1Of2 (List.rev acc |> List.map toString), Chunk l)
    | acc, [], _ -> Yield (Choice2Of2 (List.rev acc |> List.map toString), EOF)
    | acc,  l, _ -> lines (l::acc)
  lines []
  