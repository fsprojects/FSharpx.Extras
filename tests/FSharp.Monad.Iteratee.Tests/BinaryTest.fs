module FSharp.Monad.Tests.BinaryTest

open System
open FSharp.Collections
open FSharp.Collections.ByteString
open FSharp.Monad.Iteratee
open FSharp.Monad.Iteratee.Binary
open NUnit.Framework
open FsUnit

let runTest i =
  match run i with
  | Choice1Of2 e -> raise e
  | Choice2Of2 x -> x

[<Test>]
let ``test length should calculate the length of the list without modification``() =
  let actual = enumerate (create [|1uy;2uy;3uy|]) length |> runTest 
  actual |> should equal 3

let testPeekAndHead = [|
  [| box empty; box None |]
  [| box (singleton 'c'B); box (Some 'c'B) |]
  [| box (create "char"B); box (Some 'c'B) |]
|]

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream``(input, expected:byte option) =
  let actual = enumerate input peek |> runTest 
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream``(input, expected:byte option) =
  let actual = enumerate input head |> runTest
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumerate (create [| 0uy..9uy |]) drop2Head |> runTest
  actual |> should equal (Some(byte x))

[<Test>]
let ``test dropWhile should drop anything before the first space``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' 'B)
    return! head }
  let actual = enumerate (create "Hello world"B) dropWhile2Head |> runTest
  actual |> should equal (Some ' 'B)

[<Test>]
[<Sequential>]
let ``test take should take the first n items``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = create [|0uy..9uy|]
  let actual = enumerate input (take x) |> runTest
  Assert.That(actual == (ByteString.take x input))

[<Test>]
let ``test takeWhile should take anything before the first space``() =
  let input = "Hello world"B
  let actual = enumeratePure1Chunk (create input) (takeWhile ((<>) ' 'B)) |> runTest
  actual |> should equal (BS(input, 0, 5))

[<Test>]
let ``test takeUntil should correctly split the input``() =
  let input = "abcde"B
  let actual = enumeratePure1Chunk (create input) (takeUntil ((=) 'c'B)) |> runTest
  actual |> should equal (BS(input, 0, 2))

[<Test>]
let ``test heads should count the number of characters in a set of headers``() =
  let actual = enumeratePure1Chunk (ByteString.ofString "abd") (heads (ByteString.ofString "abc")) |> runTest
  actual |> should equal 2

let readLinesTests = [|
  [| box ""B; box (Choice1Of2 []:Choice<String list, String list>) |]
  [| box "\r"B; box (Choice2Of2 []:Choice<String list, String list>) |]
  [| box "\n"B; box (Choice2Of2 []:Choice<String list, String list>) |]
  [| box "\r\n"B; box (Choice2Of2 []:Choice<String list, String list>) |]
  [| box "line1"B; box (Choice1Of2 []:Choice<String list, String list>) |]
  [| box "line1\n"B; box (Choice1Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r"B; box (Choice1Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r\n"B; box (Choice1Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2"B; box (Choice1Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\n"B; box (Choice1Of2 ["line1";"line2"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\n\r\n"B; box (Choice2Of2 ["line1";"line2"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5"B; box (Choice1Of2 ["line1";"line2";"line3";"line4"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n"B
     box (Choice1Of2 ["line1";"line2";"line3";"line4";"line5"]:Choice<String list, String list>) |]
  [| box "PUT /file HTTP/1.1\r\nHost: example.com\rUser-Agent: X\nContent-Type: text/plain\r\n\r\n1C\r\nbody line 2\r\n\r\n7"B
     box (Choice2Of2 ["PUT /file HTTP/1.1";"Host: example.com";"User-Agent: X";"Content-Type: text/plain"]:Choice<String list, String list>) |]
|]

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input``(input, expected:Choice<String list, String list>) =
  let actual = enumeratePure1Chunk (create input) readLines |> runTest
  actual |> should equal expected

[<Ignore("Array index out of range error, and still incorrect when parsing")>]
[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when chunked``(input, expected:Choice<String list, String list>) =
  let actual = enumeratePureNChunk (create input) 11 (* Problem is that this is not consistent; try 5 and 10 *) readLines |> runTest
  actual |> should equal expected
