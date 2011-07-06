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

//val enumerate : 'a list -> Enumerator<'a,'b,'c>
let rec enumerate input i =
  match input, i with
  | []   , Continue(e,k) -> enumerate [] (fst (k EOF))        // generate a Yield or Error
  | x::xs, Continue(e,k) -> enumerate xs (fst (k (Chunk x)))  // generate a Continue or Error
  | _    , i             -> i                                 // return the Yield or Error

// val enumeratePure1Chunk : 'a -> Enumerator<'a,'b,'c>
let enumeratePure1Chunk str i =
  match str, i with
  | str, Continue(None, k) -> fst (k (Chunk str))
  | _  , i                 -> i

// val enumeratePure1Chunk : #seq<'a> -> int -> Enumerator<'a,'b,'c>
let rec enumeratePureNChunk (str:#seq<_>) n i =
  match str, n, i with
  | str, n, Continue(None, k) ->
      let (s1, s2) = (Seq.take n str, Seq.skip n str)
      enumeratePureNChunk s2 n (fst (k (Chunk str)))
  | _  , _, i                 -> i

//let enumStream (stream:Stream) bufferSize iter =
//  let buffer = Array.zeroCreate<byte> bufferSize
//  let rec loop iter =
//    match iter with
//    | Continue(None, k) -> read k
//    | _ -> fun p -> Yield k
//  and read k p = iteratee {
//    let! n =  }      
//  loop iter

let counter<'a> : Iteratee<'a,int> =
  let rec step n = function
    | Chunk x as s -> Continue(None, step (n + 1)), s
    | Empty   as s -> Continue(None, step n), s
    | EOF     as s -> Yield n, s
  Continue(None, step 0)

[<Test>]
let ``test counter should calculate the length of the list without modification``() =
  let actual = enumerate [1;2;3] counter
  runIteratee actual |> should equal 3

let rec peek =
  let rec step = function
    | Empty   as s -> peek, s
    | Chunk x as s -> Yield (Some x), s
    | s            -> Yield None, s
  Continue(None, step)

let testPeek = [|
  [| box ([]:char list); box None |]
  [| box ['c']; box (Some 'c') |]
  [| box ['c';'h';'a';'r']; box (Some 'c') |]
|]
[<Test>]
[<TestCaseSource("testPeek")>]
let ``test peek should return the value without removing it from the stream``(input:char list, expected:char option) =
  let actual = enumerate input peek
  runIteratee actual |> should equal expected

let rec head =
  let rec step = function
    | Empty        as s -> head, Stream.Empty
    | Chunk(x::xs) as s -> Yield x, (Chunk xs)
    | s                 -> Continue (Some (Exception("EOF")), step), s
  Continue(None, step)

let testHead = [|
  [| box ['c']; box 'c' |]
  [| box ['c';'h';'a';'r']; box 'c' |]
|]
[<Test>]
[<TestCaseSource("testHead")>]
let ``test head should return the value and remove it from the stream``(input:char list, expected:char) =
  let actual = enumerate [input] head
  runIteratee actual |> should equal expected

[<Ignore>]
[<Test>]
let ``test head should fail for an empty list``() =
  enumeratePure1Chunk [] head |> runIteratee |> should throw typeof<System.Exception>

let rec drop n =
  let rec step = function
    | Chunk x as s -> drop (n - 1), s
    | Empty   as s -> Continue(None, step), s
    | EOF     as s -> Yield (), s
  if n = 0 then Yield () else Continue(None, step)

let split (pred:char -> bool) =
  let ieContM k = Continue(None, k), Stream.Empty
  let rec step before = function
    | Empty -> ieContM (step before)
    | Chunk str ->
        match List.split pred str with
        | (_,[]) -> ieContM (step (before @ str))
        | (str,tail) -> Yield (before @ str), Chunk tail
    | s     -> Yield before, s
  Continue(None, step [])

[<Test>]
let ``test split should correctly split the input``() =
  enumeratePure1Chunk (List.ofSeq "abcde") (split ((=) 'c')) |> runIteratee |> should equal ['a';'b']

let heads str =
  let rec loop count str =
    match count, str with
    | (count, []) -> Yield count
    | (count, str) -> Continue(None, step count str)
  and step count str s =
    let str = List.ofSeq str
    match count, str, s with
    | count, str, (Chunk []) -> loop count str, (Chunk [])
    | count, c::t, (Chunk (c'::t')) ->
        if c = c' then step (count + 1) t (Chunk t') 
        else Yield count, (Chunk (c'::t'))
    | count, _, s -> Yield count, s
  loop 0 str

[<Test>]
let ``test heads should count the number of characters in a set of headers``() =
  enumeratePure1Chunk (List.ofSeq "abd") (heads (List.ofSeq "abc")) |> runIteratee |> should equal 2

let readLines =
  let toString chars = String(Array.ofList chars)
  let terminators = heads ['\r';'\n'] >>= (fun n -> if n = 0 then heads ['\n'] else Yield n)
  let rec lines acc =
    split (fun c -> c = '\r' || c = '\n') >>= fun l -> terminators >>= check acc l
  and check acc l count =
    match acc, l, count with
    | acc,  _, 0 -> Yield (Choice1Of2 (List.rev acc |> List.map toString))
    | acc, [], _ -> Yield (Choice2Of2 (List.rev acc |> List.map toString))
    | acc,  l, _ -> lines (l::acc)
  lines []
  