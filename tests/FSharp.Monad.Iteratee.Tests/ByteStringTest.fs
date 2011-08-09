module FSharp.Collections.Tests.ByteStringTest

open System
open FSharp.Collections
open NUnit.Framework
open FsUnit

[<Test>]
let ``test ByteString_take correctly truncates the ByteString at the selected index``() =
  let input = "Howdy! Want to play?"B
  let str = ByteString.create input
  let expected = BS(input, 0, 6)
  let actual = ByteString.take 6 str
  actual |> should equal expected

[<Test>]
let ``test ByteString_split correctly breaks the ByteString on the specified predicate``() =
  let input = "Howdy! Want to play?"B
  let str = ByteString.create input
  let expected = BS(input, 0, 6), BS(input, 6, 14)
  let actual = ByteString.split ((=) ' 'B) str
  actual |> should equal expected

[<Test>]
let ``test ByteString_splitAt correctly breaks the ByteString on the specified index``() =
  let input = "Howdy! Want to play?"B
  let str = ByteString.create input
  let expected = BS(input, 0, 6), BS(input, 6, 14)
  let actual = ByteString.splitAt 6 str
  actual |> should equal expected

[<Test>]
let ``test ByteString_split correctly breaks the ByteString on \r``() =
  let input = "test\r\ntest"B
  let str = ByteString.create input
  let expected = BS(input, 0, 4), BS(input, 4, 6)
  let actual = ByteString.split (fun c -> c = '\r'B || c = '\n'B) str
  actual |> should equal expected
