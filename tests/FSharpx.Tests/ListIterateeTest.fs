module FSharpx.Tests.ListIterateeTest

open System
open FSharpx
open FSharpx.Iteratee
open FSharpx.Iteratee.List
open Microsoft.FSharp.Collections
open NUnit.Framework
open FsUnit

[<Test>]
let ``test length using Enumerator_scan``() =
  let en = [1..3] |> List.toSeq |> fun x -> x.GetEnumerator()

  // `Enumerator.scan` does not allow for chunking input data.
  // It's possible to create additional operators based on `scan`.
  //
  // Another issue is that `scan` does not stop when a `Done` state
  // is achieved. Taking a slightly different approach, we could
  // implement `scanWhile` and `scanUntil`. These would work provided
  // they return any unconsumed data, in addition to the result.
  //
  // As you can see, iteratee is really no different than `Seq.scan`
  // with the state seed already embedded.
  let length = FSharpx.Enumerator.scan (fun state x -> state + 1) 0
  let ``iteratee`` = length en

  // Finally, we need a `last` function, as no such method exists.
  // The `last` function should iterate through the sequence and return
  // the final state.
  let actual = ``iteratee`` |> Enumerator.last

  actual |> should equal 3

[<Test>]
let ``test length should calculate the length of the list without modification``() =
  let actual = enumerate [1;2;3] length |> run
  actual |> should equal 3

[<Test>]
let ``test length should calculate the length of the list without modification at once``() =
  let actual = enumeratePure1Chunk [1;2;3] length |> run 
  actual |> should equal 3

[<Test>]
let ``test length should calculate the length of the list without modification when chunked``() =
  let actual = enumeratePureNChunk 2 [1;2;3] length |> run 
  actual |> should equal 3

let testPeekAndHead = [|
  [| box ([]:char list); box None |]
  [| box ['c']; box (Some 'c') |]
  [| box ['c';'h';'a';'r']; box (Some 'c') |]
|]

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream``(input:char list, expected:char option) =
  let actual = enumerate input peek |> run 
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream at once``(input:char list, expected:char option) =
  let actual = enumeratePure1Chunk input peek |> run 
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream when chunked``(input:char list, expected:char option) =
  let actual = enumeratePureNChunk 2 input peek |> run 
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream``(input:char list, expected:char option) =
  let actual = enumerate input head |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream at once``(input:char list, expected:char option) =
  let actual = enumeratePure1Chunk input head |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream when chunked``(input:char list, expected:char option) =
  let actual = enumeratePureNChunk 2 input head |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumerate [0..9] drop2Head |> run
  actual |> should equal (Some x)

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items at once``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumeratePure1Chunk [0..9] drop2Head |> run
  actual |> should equal (Some x)

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items when chunked``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumeratePureNChunk 5 [0..9] drop2Head |> run
  actual |> should equal (Some x)

[<Test>]
let ``test dropWhile should drop anything before the first space``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' ')
    return! head }
  let actual = enumerate (List.ofSeq "Hello world") dropWhile2Head |> run
  actual |> should equal (Some ' ')

[<Test>]
let ``test dropWhile should drop anything before the first space at once``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' ')
    return! head }
  let actual = enumeratePure1Chunk (List.ofSeq "Hello world") dropWhile2Head |> run
  actual |> should equal (Some ' ')

[<Test>]
let ``test dropWhile should drop anything before the first space when chunked``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' ')
    return! head }
  let actual = enumeratePureNChunk 2 (List.ofSeq "Hello world") dropWhile2Head |> run
  actual |> should equal (Some ' ')
  
[<Test>]
let ``test dropUntil should drop anything before the first space``() =
  let dropUntil2Head = iteratee {
    do! dropUntil ((=) ' ')
    return! head }
  let actual = enumerate (List.ofSeq "Hello world") dropUntil2Head |> run
  actual |> should equal (Some ' ')

[<Test>]
let ``test dropUntil should drop anything before the first space at once``() =
  let dropUntil2Head = iteratee {
    do! dropUntil ((=) ' ')
    return! head }
  let actual = enumeratePure1Chunk (List.ofSeq "Hello world") dropUntil2Head |> run
  actual |> should equal (Some ' ')

[<Test>]
let ``test dropUntil should drop anything before the first space when chunked``() =
  let dropUntil2Head = iteratee {
    do! dropUntil ((=) ' ')
    return! head }
  let actual = enumeratePureNChunk 2 (List.ofSeq "Hello world") dropUntil2Head |> run
  actual |> should equal (Some ' ')

[<Test>]
[<Sequential>]
let ``test take should take the first n items``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = [0..9]
  let expected = FSharpx.List.take x input
  let actual = enumerate input (take x) |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test take should take the first n items at once``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = [0..9]
  let expected = FSharpx.List.take x input
  let actual = enumeratePure1Chunk input (take x) |> run
  actual |> should equal expected
  
[<Test>]
[<Sequential>]
let ``test take should take the first n items when chunked``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = [0..9]
  let expected = FSharpx.List.take x input
  let actual = enumeratePureNChunk 2 input (take x) |> run
  actual |> should equal expected

[<Test>]
let ``test takeWhile should take anything before the first space``() =
  let actual = enumerate (List.ofSeq "Hello world") (takeWhile ((<>) ' ')) |> run
  actual |> should equal (List.ofSeq "Hello")

[<Test>]
let ``test takeWhile should take anything before the first space at once``() =
  let actual = enumeratePure1Chunk (List.ofSeq "Hello world") (takeWhile ((<>) ' ')) |> run
  actual |> should equal (List.ofSeq "Hello")

[<Test>]
let ``test takeWhile should take anything before the first space when chunked``() =
  let actual = enumeratePureNChunk 2 (List.ofSeq "Hello world") (takeWhile ((<>) ' ')) |> run
  actual |> should equal (List.ofSeq "Hello")

[<Test>]
let ``test takeUntil should correctly split the input``() =
  let actual = enumerate (List.ofSeq "abcde") (takeUntil ((=) 'c')) |> run
  actual |> should equal ['a';'b']

[<Test>]
let ``test takeUntil should correctly split the input at once``() =
  let actual = enumeratePure1Chunk (List.ofSeq "abcde") (takeUntil ((=) 'c')) |> run
  actual |> should equal ['a';'b']

[<Test>]
let ``test takeUntil should correctly split the input when chunked``() =
  let actual = enumeratePureNChunk 2 (List.ofSeq "abcde") (takeUntil ((=) 'c')) |> run
  actual |> should equal ['a';'b']

let takeUntilTests = [|
  [| box ""; box ([]:char list); box ([]:char list) |]
  [| box "\r"; box ([]:char list); box ['\r']|]
  [| box "\n"; box ([]:char list); box ['\n'] |]
  [| box "\r\n"; box ([]:char list); box ['\r';'\n'] |]
  [| box "line1"; box ([]:char list); box ([]:char list) |]
  [| box "line1\n"; box ['l';'i';'n';'e';'1']; box ['\n'] |]
  [| box "line1\r"; box ['l';'i';'n';'e';'1']; box ['\r']|]
  [| box "line1\r\n"; box ['l';'i';'n';'e';'1']; box ['\r';'\n'] |]
|]

[<Ignore("heads and readLines do not correctly return a correct result when the input is chunked and a \r\n is encountered in different chunks.")>]
[<Test>]
[<TestCaseSource("takeUntilTests")>]
let ``test takeUntilNewline should split strings on a newline character``(input, expectedRes:char list, expectedRem:char list) =
  let isNewline c = c = '\r' || c = '\n'
  let res, rem =
    match enumerate (List.ofSeq input) (takeUntil isNewline) with
    | Done(res, (Chunk rem)) -> res, rem
    | Continue _ -> [], []
    | _ -> failwith "Unrecognized test result"
  res |> should equal expectedRes
  rem |> should equal expectedRem

[<Test>]
[<TestCaseSource("takeUntilTests")>]
let ``test takeUntilNewline should split strings on a newline character at once``(input, expectedRes:char list, expectedRem:char list) =
  let isNewline c = c = '\r' || c = '\n'
  let res, rem =
    match enumeratePure1Chunk (List.ofSeq input) (takeUntil isNewline) with
    | Done(res, (Chunk rem)) -> res, rem
    | Continue _ -> [], []
    | _ -> failwith "Unrecognized test result"
  res |> should equal expectedRes
  rem |> should equal expectedRem

[<Test>]
let ``test heads should count the number of characters in a set of headers when enumerated one at a time``() =
  let actual = enumerate (List.ofSeq "abd") (heads (List.ofSeq "abc")) |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the number of characters in a set of headers``() =
  let actual = enumeratePure1Chunk (List.ofSeq "abd") (heads (List.ofSeq "abc")) |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the number of characters in a set of headers when chunked``() =
  let actual = enumeratePureNChunk 2 (List.ofSeq "abd") (heads (List.ofSeq "abc")) |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers when enumerated char by char``() =
  let isNewline c = c = '\r' || c = '\n'
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (List.ofSeq "\r\n")
  let actual = enumerate (List.ofSeq "abc\r\n") readUntilNewline |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers``() =
  let isNewline c = c = '\r' || c = '\n'
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (List.ofSeq "\r\n")
  let actual = enumeratePure1Chunk (List.ofSeq "abc\r\n") readUntilNewline |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers when chunked``() =
  let isNewline c = c = '\r' || c = '\n'
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (List.ofSeq "\r\n")
  let actual = enumeratePureNChunk 2 (List.ofSeq "abc\r\n") readUntilNewline |> run
  actual |> should equal 2

let readLinesTests = [|
  [| box ""; box (Choice1Of2 []:Choice<String list, String list>) |]
  [| box "\r"; box (Choice2Of2 []:Choice<String list, String list>) |]
  [| box "\n"; box (Choice2Of2 []:Choice<String list, String list>) |]
  [| box "\r\n"; box (Choice2Of2 []:Choice<String list, String list>) |]
  [| box "line1"; box (Choice1Of2 []:Choice<String list, String list>) |]
  [| box "line1\n"; box (Choice1Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r"; box (Choice1Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r\n"; box (Choice1Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2"; box (Choice1Of2 ["line1"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\n"; box (Choice1Of2 ["line1";"line2"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\n\r\n"; box (Choice2Of2 ["line1";"line2"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5"; box (Choice1Of2 ["line1";"line2";"line3";"line4"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n"
     box (Choice1Of2 ["line1";"line2";"line3";"line4";"line5"]:Choice<String list, String list>) |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n\r\n"
     box (Choice2Of2 ["line1";"line2";"line3";"line4";"line5"]:Choice<String list, String list>) |]
  [| box "PUT /file HTTP/1.1\r\nHost: example.com\nUser-Agent: X\nContent-Type: text/plain\r\n\r\n1C\r\nbody line 2\r\n\r\n7"
     box (Choice2Of2 ["PUT /file HTTP/1.1";"Host: example.com";"User-Agent: X";"Content-Type: text/plain"]:Choice<String list, String list>) |]
|]

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input``(input, expected:Choice<String list, String list>) =
  let actual = enumeratePure1Chunk (List.ofSeq input) readLines |> run
  actual |> should equal expected

[<Ignore("heads and readLines do not correctly return a correct result when the input is chunked and a \r\n is encountered in different chunks.")>]
[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when enumerated one char at a time``(input, expected:Choice<String list, String list>) =
  let actual = enumerate (List.ofSeq input) readLines |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when chunked``(input, expected:Choice<String list, String list>) =
  let actual = enumeratePureNChunk 11 (* Problem is that this is not consistent; try 5 and 10 *) (List.ofSeq input) readLines |> run
  actual |> should equal expected
