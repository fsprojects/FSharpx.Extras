module FSharpx.DataStructures.Tests.EditDistanceTest

open FSharpx
open FSharpx.DataStructures
open NUnit.Framework
open FsUnit

type BS = ByteString

[<Test>]
let ``distance example``() =
    BKTree.ByteString.distance (BS "kitten"B) (BS "sitting"B)
    |> should equal 3

[<Test>]
let ``toListDistance example``() =
    [BS "kitten"B; BS "setting"B; BS "getting"B]
    |> BKTree.ByteString.ofList
    |> BKTree.ByteString.toListDistance 2 (BS "sitting"B)
    |> should equal [BS "setting"B; BS "getting"B]