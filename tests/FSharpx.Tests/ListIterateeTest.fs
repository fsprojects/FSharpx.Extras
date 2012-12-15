module FSharpx.Tests.ListIterateeTest

open System
open FSharpx
open FSharpx.Iteratee
open FSharpx.Iteratee.List
open Microsoft.FSharp.Collections
open NUnit.Framework
open FsUnit

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
  let expected = FSharpx.Collections.List.take x input
  let actual = enumerate input (take x) |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test take should take the first n items at once``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = [0..9]
  let expected = FSharpx.Collections.List.take x input
  let actual = enumeratePure1Chunk input (take x) |> run
  actual |> should equal expected
  
[<Test>]
[<Sequential>]
let ``test take should take the first n items when chunked``([<Values(0,1,2,3,4,5,6,7,8,9,10)>] x) =
  let input = [0..9]
  let expected = FSharpx.Collections.List.take x input
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

[<Test>]
let ``test skipNewline should consume \r for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a')
  let actual = enumerate (List.ofSeq "\ra") (skipNewline *> ``take 'a'``) |> run
  actual |> should equal ['a']

[<Test>]
let ``test skipNewline should consume \n for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a')
  let actual = enumerate (List.ofSeq "\na") (skipNewline *> ``take 'a'``) |> run
  actual |> should equal ['a']

[<Test>]
let ``test skipNewline should consume \r\n for a single newline``() =
  let ``take 'a'``= takeWhile ((=) 'a')
  let actual = enumerate (List.ofSeq "\r\na") (skipNewline *> ``take 'a'``) |> run
  actual |> should equal ['a']

let readLineTests = [|
  [| box ""; box ([]:char list) |]
  [| box "\r"; box ([]:char list) |]
  [| box "\n"; box ([]:char list) |]
  [| box "\r\n"; box ([]:char list) |]
  [| box "line1"; box ['l';'i';'n';'e';'1'] |]
  [| box "line1\n"; box ['l';'i';'n';'e';'1'] |]
  [| box "line1\r"; box ['l';'i';'n';'e';'1'] |]
  [| box "line1\r\n"; box ['l';'i';'n';'e';'1'] |]
|]

[<Test>]
[<TestCaseSource("readLineTests")>]
let ``test readLine should split strings on a newline character``(input, expectedRes:char list) =
  let actual = enumerate (List.ofSeq input) readLine |> run
  actual |> should equal expectedRes

[<Test>]
[<TestCaseSource("readLineTests")>]
let ``test readLine should split strings on a newline character at once``(input, expectedRes:char list) =
  let actual = enumeratePure1Chunk (List.ofSeq input) readLine |> run
  actual |> should equal expectedRes

let readLinesTests = [|
  [| box ""; box ([]:String list) |]
  [| box "\r"; box [""] |]
  [| box "\n"; box [""] |]
  [| box "\r\n"; box [""] |]
  [| box "line1"; box ["line1"] |]
  [| box "line1\n"; box ["line1"] |]
  [| box "line1\r"; box ["line1"] |]
  [| box "line1\r\n"; box ["line1"] |]
  [| box "line1\r\nline2"; box ["line1";"line2"] |]
  [| box "line1\r\nline2\r\n"; box ["line1";"line2"] |]
  [| box "line1\r\nline2\r\n\r\n"; box ["line1";"line2";""] |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5"; box ["line1";"line2";"line3";"line4";"line5"] |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n"; box ["line1";"line2";"line3";"line4";"line5"] |]
  [| box "line1\r\nline2\r\nline3\r\nline4\r\nline5\r\n\r\n"; box ["line1";"line2";"line3";"line4";"line5";""] |]
  [| box "PUT /file HTTP/1.1\r\nHost: example.com\nUser-Agent: X\nContent-Type: text/plain\r\n\r\n1C\r\nbody line 2\r\n\r\n7"
     box ["PUT /file HTTP/1.1";"Host: example.com";"User-Agent: X";"Content-Type: text/plain";"";"1C";"body line 2";"";"7"] |]
|]

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input``(input, expected:String list) =
  let input = List.ofSeq input
  let actual = enumeratePure1Chunk input readLines |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when enumerated one char at a time``(input, expected:String list) =
  let input = List.ofSeq input
  let actual = enumerate input readLines |> run
  actual |> should equal expected

[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when chunked``(input, expected:String list) =
  let input = List.ofSeq input
  let actual = enumeratePureNChunk 5 input readLines |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test consume should consume all items``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let actual = enumerate [ 0..x ] consume |> run
  actual |> should equal [ 0..x ]

[<Test>]
[<Sequential>]
let ``test consume should consume all items at once``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let actual = enumeratePure1Chunk [ 0..x ] consume |> run
  actual |> should equal [ 0..x ]

[<Test>]
[<Sequential>]
let ``test consume should consume all items when enumerating in chunks``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let actual = enumeratePureNChunk 5 [ 0..x ] consume |> run
  actual |> should equal [ 0..x ]

[<Test>]
[<Sequential>]
let ``test isolate and consume should take the first n items from the stream``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let input = [ 0..9 ]
  let expected = FSharpx.Collections.List.take x input
  let actual = enumerate input (joinI (isolate x consume)) |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test isolate and consume should take the first n items from the stream at once``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let input = [ 0..9 ]
  let expected = FSharpx.Collections.List.take x input
  let actual = enumeratePure1Chunk input (joinI (isolate x consume)) |> run
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test isolate and consume should take the first n items when chunked``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let input = [ 0..9 ]
  let expected = FSharpx.Collections.List.take x input
  let actual = enumeratePureNChunk 5 input (joinI (isolate x consume)) |> run
  actual |> should equal expected

[<Test>]
let ``test isolateWhile and consume should take anything before the first space``() =
  let input = "Hello world".ToCharArray() |> Array.toList
  let expected = "Hello".ToCharArray() |> Array.toList
  let actual = enumerate input (joinI (isolateWhile ((<>) ' ') consume)) |> run
  actual |> should equal expected

[<Test>]
let ``test isolateWhile and consume should take anything before the first space at once``() =
  let input = "Hello world".ToCharArray() |> Array.toList
  let expected = "Hello".ToCharArray() |> Array.toList
  let actual = enumeratePure1Chunk input (joinI (isolateWhile ((<>) ' ') consume)) |> run
  actual |> should equal expected

[<Test>]
let ``test isolateWhile and consume should take anything before the first space when enumerating in chunks``() =
  let input = "Hello world".ToCharArray() |> Array.toList
  let expected = "Hello".ToCharArray() |> Array.toList
  let actual = enumeratePureNChunk 5 input (joinI (isolateWhile ((<>) ' ') consume)) |> run
  actual |> should equal expected

[<Test>]
let ``test isolateUntil and consume should correctly split the input``() =
  let input = "abcde".ToCharArray() |> Array.toList
  let expected = "ab".ToCharArray() |> Array.toList
  let actual = enumerate input (joinI (isolateUntil ((=) 'c') consume)) |> run
  actual |> should equal expected

[<Test>]
let ``test isolateUntil and consume should correctly split the input at once``() =
  let input = "abcde".ToCharArray() |> Array.toList
  let expected = "ab".ToCharArray() |> Array.toList
  let actual = enumeratePure1Chunk input (joinI (isolateUntil ((=) 'c') consume)) |> run
  actual |> should equal expected

[<Test>]
let ``test isolateUntil and consume should correctly split the input when enumerating in chunks``() =
  let input = "abcde".ToCharArray() |> Array.toList
  let expected = "ab".ToCharArray() |> Array.toList
  let actual = enumeratePureNChunk 2 input (joinI (isolateUntil ((=) 'c') consume)) |> run
  actual |> should equal expected

[<Test>]
let ``test map should map to uppercase letters to lowercase``() =
  let lower = joinI (map Char.ToLower consume)
  let actual = enumerate ['A'; 'B'; 'C'; 'D'; 'E'] lower |> run
  actual |> should equal ['a'; 'b'; 'c'; 'd'; 'e']

[<Test>]
let ``test map should map to uppercase letters to lowercase at once``() =
  let lower = joinI (map Char.ToLower consume)
  let actual = enumeratePure1Chunk ['A'; 'B'; 'C'; 'D'; 'E'] lower |> run
  actual |> should equal ['a'; 'b'; 'c'; 'd'; 'e']

[<Test>]
let ``test map should map to uppercase letters to lowercase when enumerating in chunks``() =
  let lower = joinI (map Char.ToLower consume)
  let actual = enumeratePureNChunk 2 ['A'; 'B'; 'C'; 'D'; 'E'] lower |> run
  actual |> should equal ['a'; 'b'; 'c'; 'd'; 'e']

[<Test>]
let ``test filter should filter filter the value for which the given predicate returns false``() =
  let input = "Hello world".ToCharArray() |> Array.toList
  let expected = "Heo word".ToCharArray() |> Array.toList
  let actual = enumerate input (joinI (filter ((<>) 'l') consume)) |> run
  actual |> should equal expected

[<Test>]
let ``test filter should filter the value for which the given predicate returns false at once``() =
  let input = "Hello world".ToCharArray() |> Array.toList
  let expected = "Heo word".ToCharArray() |> Array.toList
  let actual = enumeratePure1Chunk input (joinI (filter ((<>) 'l') consume)) |> run
  actual |> should equal expected

[<Test>]
let ``test filter should filter the value for which the given predicate returns false when enumerating in chunks``() =
  let input = "Hello world".ToCharArray() |> Array.toList
  let expected = "Heo word".ToCharArray() |> Array.toList
  let actual = enumeratePureNChunk 5 input (joinI (filter ((<>) 'l') consume)) |> run
  actual |> should equal expected

(* CSV Parser *)

let takeUntilComma = takeUntil ((=) ',')

[<Test>]
let ``test takeUntilComma should take until the first comma``() =
  let csvSample = List.ofSeq "blah,blah,blah"
  let actual = enumerate csvSample takeUntilComma |> run
  actual |> should equal (List.ofSeq "blah")

let readCsvLine = many (takeUntilComma <* drop 1)

[<Test>]
let ``test readCsvLine should take chunks until no commas remain``() =
  let csvSample = List.ofSeq "blah,blah,blah"
  let actual = enumerate csvSample readCsvLine |> run
  actual |> should equal [List.ofSeq "blah";List.ofSeq "blah";List.ofSeq "blah"]

[<Test>]
let ``test readCsvLine should return the empty byte string when that's all it is passed``() =
  let csvSample = []
  let actual = enumerate csvSample readCsvLine |> run
  actual |> should equal []
