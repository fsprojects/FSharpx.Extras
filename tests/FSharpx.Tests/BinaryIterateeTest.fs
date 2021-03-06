module FSharpx.Tests.BinaryIterateeTest

open FSharpx.Collections
open FSharpx.Functional.Iteratee
open FSharpx.Functional.Iteratee.Binary
open NUnit.Framework
open FsUnitTyped

type BS = ByteString

[<Test>]
let ``test length should calculate the length of the list without modification``() =
  let actual = enumerate (ByteString.create [|1uy;2uy;3uy|]) length |> run 
  actual |> shouldEqual 3

[<Test>]
let ``test length should calculate the length of the list without modification all at once``() =
  let actual = enumeratePure1Chunk (ByteString.create [|1uy;2uy;3uy|]) length |> run 
  actual |> shouldEqual 3

[<Test>]
let ``test length should calculate the length of the list without modification when chunked``() =
  let actual = enumeratePureNChunk 2 (ByteString.create [|1uy;2uy;3uy|]) length |> run 
  actual |> shouldEqual 3

let testPeekAndHead = [
  TestCaseData(ByteString.empty, ExpectedResult = None)
  TestCaseData(ByteString.singleton 'c'B, ExpectedResult = Some 'c'B)
  TestCaseData(ByteString.create "char"B, ExpectedResult = Some 'c'B)
]

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream`` (input:ByteString) : byte option =
  enumerate input peek |> run 

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream at once`` (input:ByteString) : byte option=
  enumeratePure1Chunk input peek |> run 

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream when chunked`` (input:ByteString) : byte option =
  enumeratePureNChunk 2 input peek |> run 

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream`` (input:ByteString) : byte option =
  enumerate input head |> run

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream at once`` (input:ByteString) : byte option =
  enumeratePure1Chunk input head |> run

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream when chunked`` (input:ByteString) : byte option =
  enumeratePureNChunk 2 input head |> run

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumerate (ByteString.create [| 0uy..9uy |]) drop2Head |> run
  actual |> shouldEqual (Some(byte x))

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items at once``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumeratePure1Chunk (ByteString.create [| 0uy..9uy |]) drop2Head |> run
  actual |> shouldEqual (Some(byte x))

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items when enumerating in chunks``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumeratePureNChunk 5 (ByteString.create [| 0uy..9uy |]) drop2Head |> run
  actual |> shouldEqual (Some(byte x))

[<Test>]
let ``test dropWhile should drop anything before the first space``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' 'B)
    return! head }
  let actual = enumerate (ByteString.create "Hello world"B) dropWhile2Head |> run
  actual |> shouldEqual (Some ' 'B)

[<Test>]
let ``test dropWhile should drop anything before the first space at once``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' 'B)
    return! head }
  let actual = enumeratePure1Chunk (ByteString.create "Hello world"B) dropWhile2Head |> run
  actual |> shouldEqual (Some ' 'B)

[<Test>]
let ``test dropWhile should drop anything before the first space when chunked``() =
  let dropWhile2Head = iteratee {
    do! dropWhile ((<>) ' 'B)
    return! head }
  let actual = enumeratePureNChunk 2 (ByteString.create "Hello world"B) dropWhile2Head |> run
  actual |> shouldEqual (Some ' 'B)

[<Test>]
let ``test dropUntil should drop anything before the first space``() =
  let dropUntil2Head = iteratee {
    do! dropUntil ((=) ' 'B)
    return! head }
  let actual = enumerate (ByteString.create "Hello world"B) dropUntil2Head |> run
  actual |> shouldEqual (Some ' 'B)

[<Test>]
let ``test dropUntil should drop anything before the first space at once``() =
  let dropUntil2Head = iteratee {
    do! dropUntil ((=) ' 'B)
    return! head }
  let actual = enumeratePure1Chunk (ByteString.create "Hello world"B) dropUntil2Head |> run
  actual |> shouldEqual (Some ' 'B)

[<Test>]
let ``test dropUntil should drop anything before the first space when chunked``() =
  let dropUntil2Head = iteratee {
    do! dropUntil ((=) ' 'B)
    return! head }
  let actual = enumeratePureNChunk 2 (ByteString.create "Hello world"B) dropUntil2Head |> run
  actual |> shouldEqual (Some ' 'B)
  
[<Test>]
[<Sequential>]
let ``test take should take the first n items``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = ByteString.create [|0uy..9uy|]
  let expected = ByteString.take x input
  let actual = enumerate input (take x) |> run
  actual |> shouldEqual expected

[<Test>]
[<Sequential>]
let ``test take should take the first n items at once``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = ByteString.create [|0uy..9uy|]
  let expected = ByteString.take x input
  let actual = enumeratePure1Chunk input (take x) |> run
  actual |> shouldEqual expected

[<Test>]
[<Sequential>]
let ``test take should take the first n items when chunked``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = ByteString.create [|0uy..9uy|]
  let expected = ByteString.take x input
  let actual = enumeratePureNChunk 2 input (take x) |> run
  actual |> shouldEqual expected

[<Test>]
let ``test takeWhile should take anything before the first space``() =
  let input = "Hello world"B
  let expected = BS(input, 0, 5)
  let actual = enumerate (ByteString.create input) (takeWhile ((<>) ' 'B)) |> run
  actual |> shouldEqual expected

[<Test>]
let ``test takeWhile should take anything before the first space at once``() =
  let input = "Hello world"B
  let expected = BS(input, 0, 5)
  let actual = enumeratePure1Chunk (ByteString.create input) (takeWhile ((<>) ' 'B)) |> run
  actual |> shouldEqual expected

[<Test>]
let ``test takeWhile should take anything before the first space when enumerating in chunks``() =
  let input = "Hello world"B
  let actual = enumeratePureNChunk 2 (ByteString.create input) (takeWhile ((<>) ' 'B)) |> run
  actual |> shouldEqual (BS(input, 0, 5))

[<Test>]
let ``test takeUntil should correctly split the input``() =
  let input = "abcde"B
  let actual = enumerate (ByteString.create input) (takeUntil ((=) 'c'B)) |> run
  actual |> shouldEqual (BS(input, 0, 2))

[<Test>]
let ``test takeUntil should correctly split the input at once``() =
  let input = "abcde"B
  let actual = enumeratePure1Chunk (ByteString.create input) (takeUntil ((=) 'c'B)) |> run
  actual |> shouldEqual (BS(input, 0, 2))

[<Test>]
let ``test takeUntil should correctly split the input when enumerating in chunks``() =
  let input = "abcde"B
  let actual = enumeratePureNChunk 2 (ByteString.create input) (takeUntil ((=) 'c'B)) |> run
  actual |> shouldEqual (BS(input, 0, 2))

[<Test>]
let ``test heads should count the number of characters in a set of headers when enumerated one byte at a time``() =
  let actual = enumerate (ByteString.ofString "abd") (heads (ByteString.ofString "abc")) |> run
  actual |> shouldEqual 2

[<Test>]
let ``test heads should count the number of characters in a set of headers``() =
  let actual = enumeratePure1Chunk (ByteString.ofString "abd") (heads (ByteString.ofString "abc")) |> run
  actual |> shouldEqual 2

[<Test>]
let ``test heads should count the number of characters in a set of headers when enumerating in chunks``() =
  let actual = enumeratePureNChunk 2 (ByteString.ofString "abd") (heads (ByteString.ofString "abc")) |> run
  actual |> shouldEqual 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers when enumerated one byte at a time``() =
  let isNewline c = c = '\r'B || c = '\n'B
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (ByteString.create "\r\n"B)
  let actual = enumerate (ByteString.ofString "abc\r\n") readUntilNewline |> run
  actual |> shouldEqual 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers``() =
  let isNewline c = c = '\r'B || c = '\n'B
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (ByteString.create "\r\n"B)
  let actual = enumeratePure1Chunk (ByteString.ofString "abc\r\n") readUntilNewline |> run
  actual |> shouldEqual 2

[<Test>]
let ``test heads should count the correct number of newline characters in a set of headers when chunked``() =
  let isNewline c = c = '\r'B || c = '\n'B
  let readUntilNewline = takeUntil isNewline >>= fun bs -> heads (ByteString.create "\r\n"B)
  let actual = enumeratePureNChunk 2 (ByteString.ofString "abc\r\n") readUntilNewline |> run
  actual |> shouldEqual 2

[<Test>]
let ``test skipNewline should consume \r for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a'B)
  let actual = enumerate (BS"\ra"B) (skipNewline *> ``take 'a'``) |> run
  actual |> shouldEqual (BS"a"B)

[<Test>]
let ``test skipNewline should consume \n for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a'B)
  let actual = enumerate (BS"\na"B) (skipNewline *> ``take 'a'``) |> run
  actual |> shouldEqual (BS"a"B)

[<Test>]
let ``test skipNewline should consume \r\n for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a'B)
  let actual = enumerate (BS"\r\na"B) (skipNewline *> ``take 'a'``) |> run
  actual |> shouldEqual (BS"a"B)

let readLineTests =
  [
    BS ""B,          ByteString.empty
    BS "\r"B,        ByteString.empty
    BS "\n"B,        ByteString.empty
    BS "\r\n"B,      ByteString.empty
    BS "line1"B,     BS "line1"B
    BS "line1\n"B,   BS "line1"B
    BS "line1\r"B,   BS "line1"B
    BS "line1\r\n"B, BS "line1"B
  ] |> Seq.map TestCaseData

[<Test>]
[<TestCaseSource("readLineTests")>]
let ``test readLine should split strings on a newline character``(input, expectedRes:BS) =
  let actual = enumerate input readLine |> run
  actual |> shouldEqual expectedRes

[<Test>]
[<TestCaseSource("readLineTests")>]
let ``test readLine should split strings on a newline character at once``(input, expectedRes:BS) =
  let actual = enumeratePure1Chunk input readLine |> run
  actual |> shouldEqual expectedRes

let readLinesTests =
  [
    "",                                                  []
    "\r",                                                [BS""B]
    "\n",                                                [BS""B]
    "\r\n",                                              [BS""B]
    "line1",                                             [BS"line1"B]
    "line1\n",                                           [BS"line1"B]
    "line1\r",                                           [BS"line1"B]
    "line1\r\n",                                         [BS"line1"B]
    "line1\r\nline2",                                    [BS"line1"B;BS"line2"B]
    "line1\r\nline2\r\n",                                [BS"line1"B;BS"line2"B]
    "line1\r\nline2\r\n\r\n",                            [BS"line1"B;BS"line2"B;BS""B]
    "line1\r\nline2\r\nline3\r\nline4\r\nline5",         [BS"line1"B;BS"line2"B;BS"line3"B;BS"line4"B;BS"line5"B]
    "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n",     [BS"line1"B;BS"line2"B;BS"line3"B;BS"line4"B;BS"line5"B]
    "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n\r\n", [BS"line1"B;BS"line2"B;BS"line3"B;BS"line4"B;BS"line5"B;BS""B]
    "PUT /file HTTP/1.1\r\nHost: example.com\rUser-Agent: X\nContent-Type: text/plain\r\n\r\n1C\r\nbody line 2\r\n\r\n7",
      [BS"PUT /file HTTP/1.1"B;BS"Host: example.com"B;BS"User-Agent: X"B;BS"Content-Type: text/plain"B;BS""B;BS"1C"B;BS"body line 2"B;BS""B;BS"7"B]
  ] |> Seq.map TestCaseData

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input``(input, expected:BS list) =
  let input = ByteString.ofString input
  let actual = enumeratePure1Chunk input readLines |> run
  actual |> shouldEqual expected

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when enumerated one byte at a time``(input, expected:BS list) =
  let input = ByteString.ofString input
  let actual = enumerate input readLines |> run |> sprintf "%A" 
  actual |> shouldEqual (sprintf "%A" expected)

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when chunked``(input, expected:BS list) =
  let input = ByteString.ofString input
  let actual = enumeratePureNChunk 5 input readLines |> run
  actual |> shouldEqual expected

[<Test>]
[<Sequential>]
let ``test consume should consume all items``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let actual = enumerate (ByteString.create [| 0uy..x |]) consume |> run |> ByteString.toArray
  actual |> shouldEqual [| 0uy..x |]

[<Test>]
[<Sequential>]
let ``test consume should consume all items at once``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let actual = enumeratePure1Chunk (ByteString.create [| 0uy..x |]) consume |> run |> ByteString.toArray
  actual |> shouldEqual [| 0uy..x |]

[<Test>]
[<Sequential>]
let ``test consume should consume all items when enumerating in chunks``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let actual = enumeratePureNChunk 5 (ByteString.create [| 0uy..x |]) consume |> run |> ByteString.toArray
  actual |> shouldEqual [| 0uy..x |]

[<Test>]
[<Sequential>]
let ``test isolate and consume should take the first n items from the stream``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let input = ByteString.create [| 0uy..9uy |]
  let expected = ByteString.take x input
  let actual = enumerate input (joinI (isolate x consume)) |> run 
  actual |> shouldEqual expected

[<Test>]
[<Sequential>]
let ``test isolate and consume should take the first n items from the stream at once``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let input = ByteString.create [| 0uy..9uy |]
  let expected = ByteString.take x input
  let actual = enumeratePure1Chunk input (joinI (isolate x consume)) |> run
  actual |> shouldEqual expected

[<Test>]
[<Sequential>]
let ``test isolate and consume should take the first n items when chunked``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let input = ByteString.create [| 0uy..9uy |]
  let expected = ByteString.take x input
  let actual = enumeratePureNChunk 5 input (joinI (isolate x consume)) |> run
  actual |> shouldEqual expected

[<Test>]
let ``test isolateWhile and consume should take anything before the first space``() =
  let input = "Hello world"B
  let expected = BS(input, 0, 5)
  let actual = enumerate (ByteString.create input) (joinI (isolateWhile ((<>) ' 'B) consume)) |> run
  actual |> shouldEqual expected

[<Test>]
let ``test isolateWhile and consume should take anything before the first space at once``() =
  let input = "Hello world"B
  let expected = BS(input, 0, 5)
  let actual = enumeratePure1Chunk (ByteString.create input) (joinI (isolateWhile ((<>) ' 'B) consume)) |> run
  actual |> shouldEqual expected

[<Test>]
let ``test isolateWhile and consume should take anything before the first space when enumerating in chunks``() =
  let input = "Hello world"B
  let actual = enumeratePureNChunk 2 (ByteString.create input) (joinI (isolateWhile ((<>) ' 'B) consume)) |> run
  actual |> shouldEqual (BS(input, 0, 5))

[<Test>]
let ``test isolateUntil and consume should correctly split the input``() =
  let input = "abcde"B
  let actual = enumerate (ByteString.create input) (joinI (isolateUntil ((=) 'c'B) consume)) |> run
  actual |> shouldEqual (BS(input, 0, 2))

[<Test>]
let ``test isolateUntil and consume should correctly split the input at once``() =
  let input = "abcde"B
  let actual = enumeratePure1Chunk (ByteString.create input) (joinI (isolateUntil ((=) 'c'B) consume)) |> run
  actual |> shouldEqual (BS(input, 0, 2))

[<Test>]
let ``test isolateUntil and consume should correctly split the input when enumerating in chunks``() =
  let input = "abcde"B
  let actual = enumeratePureNChunk 2 (ByteString.create input) (joinI (isolateUntil ((=) 'c'B) consume)) |> run
  actual |> shouldEqual (BS(input, 0, 2))

[<Test>]
let ``test map should map to uppercase letters to lowercase``() =
  let lower = joinI (map (fun x -> x + 0x20uy) consume)
  let actual = enumerate (BS "ABCDE"B) lower |> run
  actual |> shouldEqual (BS "abcde"B)

[<Test>]
let ``test map should map to uppercase letters to lowercase at once``() =
  let lower = joinI (map (fun x -> x + 0x20uy) consume)
  let actual = enumeratePure1Chunk (BS "ABCDE"B) lower |> run
  actual |> shouldEqual (BS "abcde"B)

[<Test>]
let ``test map should map to uppercase letters to lowercase when enumerating in chunks``() =
  let lower = joinI (map (fun x -> x + 0x20uy) consume)
  let actual = enumeratePureNChunk 2 (BS "ABCDE"B) lower |> run
  actual |> shouldEqual (BS "abcde"B)

[<Test>]
let ``test filter should filter filter the value for which the given predicate returns false``() =
  let input = BS "Hello world"B
  let expected = BS "Heo word"B
  let actual = enumerate input (joinI (filter ((<>) 0x6cuy) consume)) |> run
  actual |> shouldEqual expected

[<Test>]
let ``test filter should filter the value for which the given predicate returns false at once``() =
  let input = BS "Hello world"B
  let expected = BS "Heo word"B
  let actual = enumeratePure1Chunk input (joinI (filter ((<>) 0x6cuy) consume)) |> run
  actual |> shouldEqual expected

[<Test>]
let ``test filter should filter the value for which the given predicate returns false when enumerating in chunks``() =
  let input = BS "Hello world"B
  let expected = BS "Heo word"B
  let actual = enumeratePureNChunk 5 input (joinI (filter ((<>) 0x6cuy) consume)) |> run
  actual |> shouldEqual expected

(* CSV Parser *)

let takeUntilComma = takeUntil ((=) ','B)

[<Test>]
let ``test takeUntilComma should take until the first comma``() =
  let csvSample = BS("blah,blah,blah"B)
  let actual = enumerate csvSample takeUntilComma |> run
  actual |> shouldEqual (BS("blah"B))

let readCsvLine = many (takeUntilComma <* drop 1)

[<Test>]
let ``test readCsvLine should take chunks until no commas remain``() =
  let csvSample = BS("blah,blah,blah"B)
  let actual = enumerate csvSample readCsvLine |> run |> sprintf "%A" 
  let expected = [BS("blah"B);BS("blah"B);BS("blah"B)] |> sprintf "%A" 
  actual |> shouldEqual expected

[<Test>]
let ``test readCsvLine should return the empty byte string when that's all it is passed``() =
  let csvSample = ByteString.empty
  let actual = enumerate csvSample readCsvLine |> run
  actual |> shouldEqual []
