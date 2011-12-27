module FSharpx.Tests.IterateeTest

open System
open FSharpx
open FSharpx.ArraySegment
open FSharpx.Iteratee
open NUnit.Framework
open FsUnit

[<Test>]
let ``test opt should convert a Done iteratee containing 1 into an iteratee containing Some 1``() =
  opt (returnI 1) |> run |> should equal (Some 1)

[<Test>]
let ``test opt should convert a Continue iteratee containing 1 into an iteratee containing Some 1``() =
  opt (continueI <| fun s -> returnI 1) |> run |> should equal (Some 1)

[<Test>]
let ``test opt should convert an Error iteratee into an iteratee containing None``() =
  opt (Error <| exn "Optional should return None") |> run |> should equal None

[<Test>]
let ``test length should calculate the length of the list without modification``() =
  let actual = enumerate (create [|1uy;2uy;3uy|]) length |> run 
  actual |> should equal 3

[<Test>]
let ``test length should calculate the length of the list without modification all at once``() =
  let actual = enumeratePure1Chunk (create [|1uy;2uy;3uy|]) length |> run 
  actual |> should equal 3

[<Test>]
let ``test length should calculate the length of the list without modification when chunked``() =
  let actual = enumeratePureNChunk 2 (create [|1uy;2uy;3uy|]) length |> run 
  actual |> should equal 3

let testPeekAndHead = [|
  [| box ArraySegment.empty; box None |]
  [| box (singleton 'c'B); box (Some 'c'B) |]
  [| box (create "char"B); box (Some 'c'B) |]
|]

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream``(input, expected:byte option) =
  let actual = enumerate input peek |> run 
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream at once``(input, expected:byte option) =
  let actual = enumeratePure1Chunk input peek |> run 
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream when chunked``(input, expected:byte option) =
  let actual = enumeratePureNChunk 2 input peek |> run 
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream``(input, expected:byte option) =
  let actual = enumerate input head |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream at once``(input, expected:byte option) =
  let actual = enumeratePure1Chunk input head |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream when chunked``(input, expected:byte option) =
  let actual = enumeratePureNChunk 2 input head |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumerate (create [| 0uy..9uy |]) drop2Head |> run
  actual |> should equal (Some(byte x))

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items at once``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumeratePure1Chunk (create [| 0uy..9uy |]) drop2Head |> run
  actual |> should equal (Some(byte x))

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items when enumerating in chunks``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumeratePureNChunk 5 (create [| 0uy..9uy |]) drop2Head |> run
  actual |> should equal (Some(byte x))

[<Test>]
let ``test dropWhile should drop anything before the first space``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' 'B)
    return! head }
  let actual = enumerate (create "Hello world"B) dropWhile2Head |> run
  actual |> should equal (Some ' 'B)

[<Test>]
let ``test dropWhile should drop anything before the first space at once``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' 'B)
    return! head }
  let actual = enumeratePure1Chunk (create "Hello world"B) dropWhile2Head |> run
  actual |> should equal (Some ' 'B)

[<Test>]
let ``test dropWhile should drop anything before the first space when chunked``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' 'B)
    return! head }
  let actual = enumeratePureNChunk 2 (create "Hello world"B) dropWhile2Head |> run
  actual |> should equal (Some ' 'B)

[<Test>]
let ``test dropUntil should drop anything before the first space``() =
  let dropUntil2Head = iteratee {
    do! dropUntil ((=) ' 'B)
    return! head }
  let actual = enumerate (create "Hello world"B) dropUntil2Head |> run
  actual |> should equal (Some ' 'B)

[<Test>]
let ``test dropUntil should drop anything before the first space at once``() =
  let dropUntil2Head = iteratee {
    do! dropUntil ((=) ' 'B)
    return! head }
  let actual = enumeratePure1Chunk (create "Hello world"B) dropUntil2Head |> run
  actual |> should equal (Some ' 'B)

[<Test>]
let ``test dropUntil should drop anything before the first space when chunked``() =
  let dropUntil2Head = iteratee {
    do! dropUntil ((=) ' 'B)
    return! head }
  let actual = enumeratePureNChunk 2 (create "Hello world"B) dropUntil2Head |> run
  actual |> should equal (Some ' 'B)
  
[<Test>]
[<Sequential>]
let ``test take should take the first n items``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = create [|0uy..9uy|]
  let expected = ArraySegment.take x input
  let actual = enumerate input (take x) |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test take should take the first n items at once``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = create [|0uy..9uy|]
  let expected = ArraySegment.take x input
  let actual = enumeratePure1Chunk input (take x) |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test take should take the first n items when chunked``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = create [|0uy..9uy|]
  let expected = ArraySegment.take x input
  let actual = enumeratePureNChunk 2 input (take x) |> run
  actual |> should equal expected

[<Test>]
let ``test takeWhile should take anything before the first space``() =
  let input = "Hello world"B
  let expected = ArraySegment<_>(input, 0, 5)
  let actual = enumerate (create input) (takeWhile ((<>) ' 'B)) |> run
  actual |> should equal expected

[<Test>]
let ``test takeWhile should take anything before the first space at once``() =
  let input = "Hello world"B
  let expected = ArraySegment<_>(input, 0, 5)
  let actual = enumeratePure1Chunk (create input) (takeWhile ((<>) ' 'B)) |> run
  actual |> should equal expected

[<Test>]
let ``test takeWhile should take anything before the first space when enumerating in chunks``() =
  let input = "Hello world"B
  let actual = enumeratePureNChunk 2 (create input) (takeWhile ((<>) ' 'B)) |> run
  actual |> should equal (ArraySegment<_>(input, 0, 5))

[<Test>]
let ``test takeUntil should correctly split the input``() =
  let input = "abcde"B
  let actual = enumerate (create input) (takeUntil ((=) 'c'B)) |> run
  actual |> should equal (ArraySegment<_>(input, 0, 2))

[<Test>]
let ``test takeUntil should correctly split the input at once``() =
  let input = "abcde"B
  let actual = enumeratePure1Chunk (create input) (takeUntil ((=) 'c'B)) |> run
  actual |> should equal (ArraySegment<_>(input, 0, 2))

[<Test>]
let ``test takeUntil should correctly split the input when enumerating in chunks``() =
  let input = "abcde"B
  let actual = enumeratePureNChunk 2 (create input) (takeUntil ((=) 'c'B)) |> run
  actual |> should equal (ArraySegment<_>(input, 0, 2))

[<Test>]
let ``test heads should count the number of characters in a set of headers when enumerated one byte at a time``() =
  let actual = enumerate (ArraySegment.ofString "abd") (heads (ArraySegment.ofString "abc")) |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the number of characters in a set of headers``() =
  let actual = enumeratePure1Chunk (ArraySegment.ofString "abd") (heads (ArraySegment.ofString "abc")) |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the number of characters in a set of headers when enumerating in chunks``() =
  let actual = enumeratePureNChunk 2 (ArraySegment.ofString "abd") (heads (ArraySegment.ofString "abc")) |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers when enumerated one byte at a time``() =
  let isNewline c = c = '\r'B || c = '\n'B
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (create "\r\n"B)
  let actual = enumerate (ArraySegment.ofString "abc\r\n") readUntilNewline |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers``() =
  let isNewline c = c = '\r'B || c = '\n'B
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (create "\r\n"B)
  let actual = enumeratePure1Chunk (ArraySegment.ofString "abc\r\n") readUntilNewline |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers when chunked``() =
  let isNewline c = c = '\r'B || c = '\n'B
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (create "\r\n"B)
  let actual = enumeratePureNChunk 2 (ArraySegment.ofString "abc\r\n") readUntilNewline |> run
  actual |> should equal 2

[<Test>]
let ``test skipNewline should consume \r for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a'B)
  let actual = enumerate (create "\ra"B) (skipNewline *> ``take 'a'``) |> run
  actual |> should equal (create "a"B)

[<Test>]
let ``test skipNewline should consume \n for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a'B)
  let actual = enumerate (create "\na"B) (skipNewline *> ``take 'a'``) |> run
  actual |> should equal (create "a"B)

[<Test>]
let ``test skipNewline should consume \r\n for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a'B)
  let actual = enumerate (create "\r\na"B) (skipNewline *> ``take 'a'``) |> run
  actual |> should equal (create "a"B)

let readLineTests = [|
  [| box (create ""B); box ArraySegment.empty |]
  [| box (create "\r"B); box ArraySegment.empty |]
  [| box (create "\n"B); box ArraySegment.empty |]
  [| box (create "\r\n"B); box ArraySegment.empty |]
  [| box (create "line1"B); box (create "line1"B) |]
  [| box (create "line1\n"B); box (create "line1"B) |]
  [| box (create "line1\r"B); box (create "line1"B) |]
  [| box (create "line1\r\n"B); box (create "line1"B) |]
|]

[<Test>]
[<TestCaseSource("readLineTests")>]
let ``test readLine should split strings on a newline character``(input, expectedRes:ArraySegment<_>) =
  let actual = enumerate input readLine |> run
  actual |> should equal expectedRes

[<Test>]
[<TestCaseSource("readLineTests")>]
let ``test readLine should split strings on a newline character at once``(input, expectedRes:ArraySegment<_>) =
  let actual = enumeratePure1Chunk input readLine |> run
  actual |> should equal expectedRes

let readLinesTests = [|
  [| box ""; box ([]:ArraySegment<_> list) |]
  [| box "\r"; box [create ""B] |]
  [| box "\n"; box [create ""B] |]
  [| box "\r\n"; box [create ""B] |]
  [| box "line1"; box [create "line1"B] |]
  [| box "line1\n"; box [create "line1"B] |]
  [| box "line1\r"; box [create "line1"B] |]
  [| box "line1\r\n"; box [create "line1"B] |]
  [| box "line1\r\nline2"; box [create "line1"B;create "line2"B] |]
  [| box "line1\r\nline2\r\n"; box [create "line1"B;create "line2"B] |]
  [| box "line1\r\nline2\r\n\r\n"; box [create "line1"B;create "line2"B;create ""B] |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5"; box [create "line1"B;create "line2"B;create "line3"B;create "line4"B;create "line5"B] |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n"; box [create "line1"B;create "line2"B;create "line3"B;create "line4"B;create "line5"B] |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n\r\n"; box [create "line1"B;create "line2"B;create "line3"B;create "line4"B;create "line5"B;create ""B] |]
  [| box "PUT /file HTTP/1.1\r\nHost: example.com\rUser-Agent: X\nContent-Type: text/plain\r\n\r\n1C\r\nbody line 2\r\n\r\n7"
     box [create "PUT /file HTTP/1.1"B;create "Host: example.com"B;create "User-Agent: X"B;create "Content-Type: text/plain"B;create ""B;create "1C"B;create "body line 2"B;create ""B;create "7"B] |]
|]

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input``(input, expected:ArraySegment<_> list) =
  let input = ArraySegment.ofString input
  let actual = enumeratePure1Chunk input readLines |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when enumerated one byte at a time``(input, expected:ArraySegment<_> list) =
  let input = ArraySegment.ofString input
  let actual = enumerate input readLines |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when chunked``(input, expected:ArraySegment<_> list) =
  let input = ArraySegment.ofString input
  let actual = enumeratePureNChunk 5 input readLines |> run
  actual |> should equal expected


(* CSV Parser *)

let takeUntilComma<'a> : Iteratee<'a,_> = takeUntil ((=) ','B)

[<Test>]
let ``test takeUntilComma should take until the first comma``() =
  let csvSample = ArraySegment<_>("blah,blah,blah"B)
  let actual = enumerate csvSample takeUntilComma |> run
  actual |> should equal (ArraySegment<_>("blah"B))

let readCsvLine<'a> : Iteratee<'a,_> = many (takeUntilComma <* drop 1)

[<Test>]
let ``test readCsvLine should take chunks until no commas remain``() =
  let csvSample = ArraySegment<_>("blah,blah,blah"B)
  let actual = enumerate csvSample readCsvLine |> run
  actual |> should equal [ArraySegment<_>("blah"B);ArraySegment<_>("blah"B);ArraySegment<_>("blah"B)]

[<Test>]
let ``test readCsvLine should return the empty byte string when that's all it is passed``() =
  let csvSample = ArraySegment.empty
  let actual = enumerate csvSample readCsvLine |> run
  actual |> should equal ArraySegment.empty
