module FSharpx.Tests.BinaryIterateeTest

open System
open FSharpx
open FSharpx.ByteString
open FSharpx.Iteratee
open FSharpx.Iteratee.Binary
open NUnit.Framework
open FsUnit

type BS = ByteString

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
  [| box ByteString.empty; box None |]
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
  let expected = ByteString.take x input
  let actual = enumerate input (take x) |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test take should take the first n items at once``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = create [|0uy..9uy|]
  let expected = ByteString.take x input
  let actual = enumeratePure1Chunk input (take x) |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test take should take the first n items when chunked``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = create [|0uy..9uy|]
  let expected = ByteString.take x input
  let actual = enumeratePureNChunk 2 input (take x) |> run
  actual |> should equal expected

[<Test>]
let ``test takeWhile should take anything before the first space``() =
  let input = "Hello world"B
  let expected = BS(input, 0, 5)
  let actual = enumerate (create input) (takeWhile ((<>) ' 'B)) |> run
  actual |> should equal expected

[<Test>]
let ``test takeWhile should take anything before the first space at once``() =
  let input = "Hello world"B
  let expected = BS(input, 0, 5)
  let actual = enumeratePure1Chunk (create input) (takeWhile ((<>) ' 'B)) |> run
  actual |> should equal expected

[<Test>]
let ``test takeWhile should take anything before the first space when enumerating in chunks``() =
  let input = "Hello world"B
  let actual = enumeratePureNChunk 2 (create input) (takeWhile ((<>) ' 'B)) |> run
  actual |> should equal (BS(input, 0, 5))

[<Test>]
let ``test takeUntil should correctly split the input``() =
  let input = "abcde"B
  let actual = enumerate (create input) (takeUntil ((=) 'c'B)) |> run
  actual |> should equal (BS(input, 0, 2))

[<Test>]
let ``test takeUntil should correctly split the input at once``() =
  let input = "abcde"B
  let actual = enumeratePure1Chunk (create input) (takeUntil ((=) 'c'B)) |> run
  actual |> should equal (BS(input, 0, 2))

[<Test>]
let ``test takeUntil should correctly split the input when enumerating in chunks``() =
  let input = "abcde"B
  let actual = enumeratePureNChunk 2 (create input) (takeUntil ((=) 'c'B)) |> run
  actual |> should equal (BS(input, 0, 2))

[<Test>]
let ``test heads should count the number of characters in a set of headers when enumerated one byte at a time``() =
  let actual = enumerate (ByteString.ofString "abd") (heads (ByteString.ofString "abc")) |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the number of characters in a set of headers``() =
  let actual = enumeratePure1Chunk (ByteString.ofString "abd") (heads (ByteString.ofString "abc")) |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the number of characters in a set of headers when enumerating in chunks``() =
  let actual = enumeratePureNChunk 2 (ByteString.ofString "abd") (heads (ByteString.ofString "abc")) |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers when enumerated one byte at a time``() =
  let isNewline c = c = '\r'B || c = '\n'B
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (create "\r\n"B)
  let actual = enumerate (ByteString.ofString "abc\r\n") readUntilNewline |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers``() =
  let isNewline c = c = '\r'B || c = '\n'B
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (create "\r\n"B)
  let actual = enumeratePure1Chunk (ByteString.ofString "abc\r\n") readUntilNewline |> run
  actual |> should equal 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers when chunked``() =
  let isNewline c = c = '\r'B || c = '\n'B
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (create "\r\n"B)
  let actual = enumeratePureNChunk 2 (ByteString.ofString "abc\r\n") readUntilNewline |> run
  actual |> should equal 2

[<Test>]
let ``test skipNewline should consume \r for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a'B)
  let actual = enumerate (BS"\ra"B) (skipNewline *> ``take 'a'``) |> run
  actual |> should equal (BS"a"B)

[<Test>]
let ``test skipNewline should consume \n for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a'B)
  let actual = enumerate (BS"\na"B) (skipNewline *> ``take 'a'``) |> run
  actual |> should equal (BS"a"B)

[<Test>]
let ``test skipNewline should consume \r\n for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a'B)
  let actual = enumerate (BS"\r\na"B) (skipNewline *> ``take 'a'``) |> run
  actual |> should equal (BS"a"B)

let readLineTests = [|
  [| box (BS""B); box ByteString.empty |]
  [| box (BS"\r"B); box ByteString.empty |]
  [| box (BS"\n"B); box ByteString.empty |]
  [| box (BS"\r\n"B); box ByteString.empty |]
  [| box (BS"line1"B); box (BS"line1"B) |]
  [| box (BS"line1\n"B); box (BS"line1"B) |]
  [| box (BS"line1\r"B); box (BS"line1"B) |]
  [| box (BS"line1\r\n"B); box (BS"line1"B) |]
|]

[<Test>]
[<TestCaseSource("readLineTests")>]
let ``test readLine should split strings on a newline character``(input, expectedRes:BS) =
  let actual = enumerate input readLine |> run
  actual |> should equal expectedRes

[<Test>]
[<TestCaseSource("readLineTests")>]
let ``test readLine should split strings on a newline character at once``(input, expectedRes:BS) =
  let actual = enumeratePure1Chunk input readLine |> run
  actual |> should equal expectedRes

let readLinesTests = [|
  [| box ""; box ([]:BS list) |]
  [| box "\r"; box [BS""B] |]
  [| box "\n"; box [BS""B] |]
  [| box "\r\n"; box [BS""B] |]
  [| box "line1"; box [BS"line1"B] |]
  [| box "line1\n"; box [BS"line1"B] |]
  [| box "line1\r"; box [BS"line1"B] |]
  [| box "line1\r\n"; box [BS"line1"B] |]
  [| box "line1\r\nline2"; box [BS"line1"B;BS"line2"B] |]
  [| box "line1\r\nline2\r\n"; box [BS"line1"B;BS"line2"B] |]
  [| box "line1\r\nline2\r\n\r\n"; box [BS"line1"B;BS"line2"B;BS""B] |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5"; box [BS"line1"B;BS"line2"B;BS"line3"B;BS"line4"B;BS"line5"B] |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n"; box [BS"line1"B;BS"line2"B;BS"line3"B;BS"line4"B;BS"line5"B] |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n\r\n"; box [BS"line1"B;BS"line2"B;BS"line3"B;BS"line4"B;BS"line5"B;BS""B] |]
  [| box "PUT /file HTTP/1.1\r\nHost: example.com\rUser-Agent: X\nContent-Type: text/plain\r\n\r\n1C\r\nbody line 2\r\n\r\n7"
     box [BS"PUT /file HTTP/1.1"B;BS"Host: example.com"B;BS"User-Agent: X"B;BS"Content-Type: text/plain"B;BS""B;BS"1C"B;BS"body line 2"B;BS""B;BS"7"B] |]
|]

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input``(input, expected:BS list) =
  let input = ByteString.ofString input
  let actual = enumeratePure1Chunk input readLines |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when enumerated one byte at a time``(input, expected:BS list) =
  let input = ByteString.ofString input
  let actual = enumerate input readLines |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when chunked``(input, expected:BS list) =
  let input = ByteString.ofString input
  let actual = enumeratePureNChunk 5 input readLines |> run
  actual |> should equal expected


(* CSV Parser *)

let takeUntilComma = takeUntil ((=) ','B)

[<Test>]
let ``test takeUntilComma should take until the first comma``() =
  let csvSample = BS("blah,blah,blah"B)
  let actual = enumerate csvSample takeUntilComma |> run
  actual |> should equal (BS("blah"B))

let readCsvLine = many (takeUntilComma <* drop 1)

[<Test>]
let ``test readCsvLine should take chunks until no commas remain``() =
  let csvSample = BS("blah,blah,blah"B)
  let actual = enumerate csvSample readCsvLine |> run
  actual |> should equal [BS("blah"B);BS("blah"B);BS("blah"B)]

[<Test>]
let ``test readCsvLine should return the empty byte string when that's all it is passed``() =
  let csvSample = ByteString.empty
  let actual = enumerate csvSample readCsvLine |> run
  actual |> should equal ByteString.empty
