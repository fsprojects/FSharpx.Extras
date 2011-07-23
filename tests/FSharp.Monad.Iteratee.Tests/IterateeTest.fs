module FSharp.Monad.Tests.IterateeTest

open System
open System.Linq
open FSharp.Monad
open FSharp.Monad.Iteratee
open FSharp.Monad.Iteratee.Operators
open FSharp.Monad.Enumerator
open FSharp.Monad.Enumerator.List
open FSharp.Monad.Enumerator.IO
open NUnit.Framework
open FsUnit

let runTest i =
  match run i with
  | Choice1Of2 e -> raise e
  | Choice2Of2 x -> x

let counter<'a> : Iteratee<'a list,int> =
  let rec step n = function
    | Empty | Chunk [] -> Continue (step n)
    | Chunk x          -> Continue (step (n + 1))
    | EOF         as s -> Yield(n, s)
  Continue (step 0)

[<Test>]
let ``test counter should calculate the length of the list without modification``() =
  let actual = enumerate [1;2;3] counter |> runTest 
  actual |> should equal 3

let rec peek =
  let rec step = function
    | Empty | Chunk []  -> peek
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
  let actual = enumerate input peek |> runTest 
  actual |> should equal expected

let rec head =
  let rec step = function
    | Empty | Chunk [] -> head
    | Chunk(x::xs)     -> Yield(Some x, (Chunk xs))
    | EOF              -> Yield(None, EOF)
  Continue step

let testHead = [|
  [| box ([]:char list); box None |]
  [| box ['c']; box (Some 'c') |]
  [| box ['c';'h';'a';'r']; box (Some 'c') |]
|]
[<Test>]
[<TestCaseSource("testHead")>]
let ``test head should return the value and remove it from the stream``(input:char list, expected:char option) =
  let actual = enumerate input head |> runTest
  actual |> should equal expected

let rec drop n =
  let rec step = function
    | Empty | Chunk [] -> Continue step
    | Chunk x          -> drop (n - 1)
    | EOF         as s -> Yield((), s)
  if n = 0 then Yield((), Chunk []) else Continue step

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumerate [0..9] drop2Head |> runTest
  actual |> should equal (Some x)

let split (pred:char -> bool) =
  let rec step before = function
    | Empty | Chunk [] -> Continue (step before)
    | Chunk str ->
        match List.split pred str with
        | (_,[]) -> Continue (step (before @ str))
        | (str,tail) -> Yield((before @ str), Chunk tail)
    | s -> Yield(before, s)
  Continue (step [])

[<Test>]
let ``test split should correctly split the input``() =
  let actual = enumeratePure1Chunk (List.ofSeq "abcde") (split ((=) 'c')) |> runTest
  actual |> should equal ['a';'b']

let heads str =
  let rec loop count str =
    match count, str with
    | (count, []) -> Yield(count, EOF)
    | (count, str) -> Continue (step count str)
  and step count str s =
    let str = List.ofSeq str
    match count, str, s with
    | count, str, Empty -> loop count str
    | count, str, (Chunk []) -> loop count str
    | count, c::t, (Chunk (c'::t')) ->
        if c = c' then step (count + 1) t (Chunk t') 
        else Yield(count, (Chunk (c'::t')))
    | count, _, s -> Yield(count, s)
  loop 0 str

[<Test>]
let ``test heads should count the number of characters in a set of headers``() =
  let actual = enumeratePure1Chunk (List.ofSeq "abd") (heads (List.ofSeq "abc")) |> runTest
  actual |> should equal 2

let readLines =
  let toString chars = String(Array.ofList chars)
  let terminators = heads ['\r';'\n'] >>= (fun n -> if n = 0 then heads ['\n'] else Yield(n, Chunk []))
  let rec lines acc =
    split (fun c -> c = '\r' || c = '\n') >>= fun l -> terminators >>= check acc l
  and check acc l count =
    match acc, l, count with
    | acc, [], _ -> Yield (Choice2Of2 (List.rev acc |> List.map toString), EOF)
    | acc,  l, 0 -> Yield (Choice1Of2 (List.rev acc |> List.map toString), Chunk l)
    | acc,  l, _ -> lines (l::acc)
  lines []

let readLinesTests = [|
  [| box ""; box (Choice2Of2 []:Choice<String list, String list>) |]
  [| box "line1"; box (Choice1Of2 []:Choice<String list, String list>) |]
  [| box "line1\n"; box (Choice2Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r"; box (Choice2Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r\n"; box (Choice2Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2"; box (Choice1Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\n"; box (Choice2Of2 ["line1";"line2"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5"; box (Choice1Of2 ["line1";"line2";"line3";"line4"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n"
     box (Choice2Of2 ["line1";"line2";"line3";"line4";"line5"]:Choice<String list, String list>) |]
  [| box "PUT /file HTTP/1.1\r\nHost: example.com\rUser-Agent: X\nContent-Type: text/plain\r\n\r\n1C\r\nbody line 2\r\n\r\n7"
     box (Choice2Of2 ["PUT /file HTTP/1.1";"Host: example.com";"User-Agent: X";"Content-Type: text/plain"]:Choice<String list, String list>) |]
|]
[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input``(input, expected:Choice<String list, String list>) =
  let actual = enumeratePure1Chunk (List.ofSeq input) readLines |> runTest
  actual |> should equal expected

[<Ignore("Get enumeratePureNChunk to correctly parse \r and \n as newline markers on their own.")>]
[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when chunked``(input, expected:Choice<String list, String list>) =
  let actual = enumeratePureNChunk (List.ofSeq input) 5 readLines |> runTest
  actual |> should equal expected
