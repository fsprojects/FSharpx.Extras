module FSharp.Collections.Tests.ByteStringTest

open System
open FSharp.Collections
open NUnit.Framework
open FsUnit

[<Test>]
let ``test ByteString_length should return the length of the byte string``() =
  let input = ByteString.create "Hello, world!"B
  let actual = ByteString.length input
  actual |> should equal 13

[<Test>]
let ``test ByteString_span correctly breaks the ByteString on the specified predicate``() =
  let input = "Howdy! Want to play?"B
  let str = ByteString.create input
  let expected = BS(input, 0, 6), BS(input, 6, 14)
  let actual = ByteString.span ((<>) ' 'B) str
  actual |> should equal expected

[<Test>]
let ``test ByteString_span correctly breaks the ByteString on \r``() =
  let input = "test\r\ntest"B
  let str = ByteString.create input
  let expected = BS(input, 0, 4), BS(input, 4, 6)
  let actual = ByteString.span (fun c -> c <> '\r'B && c <> '\n'B) str
  actual |> should equal expected

[<Test>]
let ``test ByteString_split correctly breaks the ByteString on the specified predicate``() =
  let input = "Howdy! Want to play?"B
  let str = ByteString.create input
  let expected = BS(input, 0, 6), BS(input, 6, 14)
  let actual = ByteString.split ((=) ' 'B) str
  actual |> should equal expected

[<Test>]
let ``test ByteString_split correctly breaks the ByteString on \r``() =
  let input = "test\r\ntest"B
  let str = ByteString.create input
  let expected = BS(input, 0, 4), BS(input, 4, 6)
  let actual = ByteString.split (fun c -> c = '\r'B || c = '\n'B) str
  actual |> should equal expected

[<Test>]
let ``test ByteString_splitAt correctly breaks the ByteString on the specified index``() =
  let input = "Howdy! Want to play?"B
  let str = ByteString.create input
  let expected = BS(input, 0, 6), BS(input, 6, 14)
  let actual = ByteString.splitAt 6 str
  actual |> should equal expected

[<Test>]
let ``test ByteString_fold should concatenate bytes into a string``() =
  ByteString.create "Howdy"B
  |> ByteString.fold (fun a b -> a + (char b).ToString()) ""
  |> should equal "Howdy"

[<Test>]
let ``test ByteString_take correctly truncates the ByteString at the selected index``() =
  let input = "Howdy! Want to play?"B
  let str = ByteString.create input
  let expected = BS(input, 0, 6)
  let actual = ByteString.take 6 str
  actual |> should equal expected

[<Test>]
[<Sequential>]
let ``test drop should drop the first n items``([<Values(0,1,2,3,4,5,6,7,8,9)>] x) =
  let input = "Howdy! Want to play?"B
  let actual = ByteString.skip 7 (ByteString.create input)
  actual |> should equal (BS(input,7,13))

[<Test>]
let ``test dropWhile should drop anything before the first space``() =
  let input = ByteString.create "Howdy! Want to play?"B
  let dropWhile2Head = ByteString.skipWhile ((<>) ' 'B) >> ByteString.head
  let actual = dropWhile2Head input
  actual |> should equal ' 'B

[<Test>]
[<Sequential>]
let ``test take should take the first n items``([<Values(1,2,3,4,5,6,7,8,9,10)>] x) =
  let bytes = [|0uy..9uy|]
  let input = ByteString.create bytes 
  let actual = ByteString.take x input
  actual |> should equal (BS(bytes,0,x))

[<Test>]
let ``test takeWhile should take anything before the first space``() =
  let input = "Hello world"B
  let actual = ByteString.takeWhile ((<>) ' 'B) (ByteString.create input)
  actual |> should equal (BS(input, 0, 5))

[<Test>]
let ``test takeUntil should correctly split the input``() =
  let input = "abcde"B
  let actual = ByteString.takeUntil ((=) 'c'B) (ByteString.create input)
  actual |> should equal (BS(input, 0, 2))
