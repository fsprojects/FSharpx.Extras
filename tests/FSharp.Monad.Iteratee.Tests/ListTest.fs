module FSharp.Monad.Tests.ListTest

open System
open FSharp.Monad.Iteratee
open FSharp.Monad.Iteratee.List
open FSharp.Monad.Iteratee.Core.Operators
open NUnit.Framework
open FsUnit

[<Test>]
let ``test List_split correctly breaks the list on the specified predicate``() =
  let str = List.ofSeq "Howdy! Want to play?"
  let expected = (List.ofSeq "Howdy!", List.ofSeq " Want to play?")
  List.split (fun c -> c = ' ') str |> should equal expected

[<Test>]
let ``test List_splitAt correctly breaks the list on the specified index``() =
  let str = List.ofSeq "Howdy! Want to play?"
  let expected = (List.ofSeq "Howdy!", List.ofSeq " Want to play?")
  List.splitAt 6 str |> should equal expected

let runTest i =
  match run i with
  | Choice1Of2 e -> raise e
  | Choice2Of2 x -> x

[<Test>]
let ``test length should calculate the length of the list without modification``() =
  let actual = enumerate [1;2;3] length |> runTest 
  actual |> should equal 3

let testPeekAndHead = [|
  [| box ([]:char list); box None |]
  [| box ['c']; box (Some 'c') |]
  [| box ['c';'h';'a';'r']; box (Some 'c') |]
|]

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test peek should return the value without removing it from the stream``(input:char list, expected:char option) =
  let actual = enumerate input peek |> runTest 
  actual |> should equal expected

[<Test>]
[<TestCaseSource("testPeekAndHead")>]
let ``test head should return the value and remove it from the stream``(input:char list, expected:char option) =
  let actual = enumerate input head |> runTest
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let drop2Head = iteratee {
    do! drop x
    return! head }
  let actual = enumerate [0..9] drop2Head |> runTest
  actual |> should equal (Some x)

[<Test>]
let ``test split should correctly split the input``() =
  let actual = enumeratePure1Chunk (List.ofSeq "abcde") (split ((=) 'c')) |> runTest
  actual |> should equal ['a';'b']

let splitTests = [|
  [| box ""; box ([]:char list); box ([]:char list) |]
  [| box "\r"; box ([]:char list); box ['\r']|]
  [| box "\n"; box ([]:char list); box ['\n'] |]
  [| box "\r\n"; box ([]:char list); box ['\r';'\n'] |]
  [| box "line1"; box ([]:char list); box ([]:char list) |]
  [| box "line1\n"; box ['l';'i';'n';'e';'1']; box ['\n'] |]
  [| box "line1\r"; box ['l';'i';'n';'e';'1']; box ['\r']|]
  [| box "line1\r\n"; box ['l';'i';'n';'e';'1']; box ['\r';'\n'] |]
|]

[<Test>]
[<TestCaseSource("splitTests")>]
let ``test splitOnNewline should split strings on a newline character``(input, expectedRes:char list, expectedRem:char list) =
  let isNewline c = c = '\r' || c = '\n'
  let res, rem =
    match enumeratePure1Chunk (List.ofSeq input) (split isNewline) with
    | Yield(res, (Chunk rem)) -> res, rem
    | Continue _ -> [], []
  res |> should equal expectedRes
  rem |> should equal expectedRem

[<Test>]
let ``test heads should count the number of characters in a set of headers``() =
  let actual = enumeratePure1Chunk (List.ofSeq "abd") (heads (List.ofSeq "abc")) |> runTest
  actual |> should equal 2

[<TestCase("", 0)>]
[<TestCase("\r", 1)>]
[<TestCase("\n", 1)>]
[<TestCase("\r\n", 2)>] 
let ``test readTerminators should return the correct number of heads for each item``(input, expected) =
  let newlines = ['\r';'\n']
  let newline = ['\n']
  let terminators = heads newlines >>= fun n -> if n = 0 then heads newline else Yield(n, Stream.Empty)
  let actual = enumeratePure1Chunk (List.ofSeq input) terminators |> runTest
  actual |> should equal expected

[<TestCase("", 0)>]
[<TestCase("\r", 1)>]
[<TestCase("\n", 1)>]
[<TestCase("\r\n", 2)>] 
[<TestCase("line1", 0)>]
[<TestCase("line1\r", 1)>]
[<TestCase("line1\n", 1)>]
[<TestCase("line1\r\n", 2)>] 
let ``test splitReadTerminators should return the correct number of heads for each item``(input, expected) =
  let newlines = ['\r';'\n']
  let newline = ['\n']
  let isNewline c = c = '\r' || c = '\n'
  let terminators = heads newlines >>= fun n -> if n = 0 then heads newline else Yield(n, Stream.Empty)
  let splitOnTerminators = split isNewline >>= fun l -> terminators
  let actual = enumeratePure1Chunk (List.ofSeq input) splitOnTerminators |> runTest
  actual |> should equal expected

[<TestCase("", 0)>]
[<TestCase("\r", 1)>]
[<TestCase("\n", 1)>]
[<TestCase("\r\n", 2)>] 
[<TestCase("line1", 0)>]
[<TestCase("line1\r", 0)>]
[<TestCase("line1\n", 0)>]
[<TestCase("line1\r\n", 0)>] 
[<TestCase("line1\rline1", 0)>]
[<TestCase("line1\rline1\r", 0)>]
[<TestCase("line1\rline1\n", 0)>]
[<TestCase("line1\rline1\r\n", 0)>] 
[<TestCase("line1\nline1", 0)>]
[<TestCase("line1\nline1\r", 0)>]
[<TestCase("line1\nline1\n", 0)>]
[<TestCase("line1\nline1\r\n", 0)>] 
[<TestCase("line1\r\nline1", 0)>]
[<TestCase("line1\r\nline1\r", 0)>]
[<TestCase("line1\r\nline1\n", 0)>]
[<TestCase("line1\r\nline1\r\n", 0)>] 
[<TestCase("line1\r\nline1\r\r", 1)>]
[<TestCase("line1\r\nline1\n\r", 1)>] 
[<TestCase("line1\r\nline1\n\n", 1)>] 
[<TestCase("line1\r\nline1\r\r\n", 2)>]
[<TestCase("line1\r\nline1\n\r\n", 2)>]
[<TestCase("line1\r\nline1\r\n\r\n", 2)>] 
let ``test recursive splitReadTerminators should return the correct number of heads for each item``(input, expected) =
  let newlines = ['\r';'\n']
  let newline = ['\n']
  let isNewline c = c = '\r' || c = '\n'
  let terminators = heads newlines >>= fun n -> if n = 0 then heads newline else Yield(n, Stream.Empty)
  let rec lines acc = split isNewline >>= fun l -> terminators >>= check acc l
  and check acc l count =
    match l, count with
    | _, 0 -> Yield (0, Chunk l)
    | [], _ -> Yield (count, EOF)
    | l, _ -> lines (l::acc)
    
  let actual = enumeratePure1Chunk (List.ofSeq input) (lines []) |> runTest
  actual |> should equal expected

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
  let actual = enumeratePure1Chunk (List.ofSeq input) readLines |> runTest
  actual |> should equal expected

[<Ignore("readLines doesn't carry trailing newline characters, so it's possible to have a trailing \r and a beginning \n that should go together.")>]
[<Test>]
[<TestCaseSource("readLinesTests")>]
let ``test readLines should return the lines from the input when chunked``(input, expected:Choice<String list, String list>) =
  let actual = enumeratePureNChunk (List.ofSeq input) 5 readLines |> runTest
  actual |> should equal expected
