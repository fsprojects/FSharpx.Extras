// originally from https://bitbucket.org/colinbul/fsharpent
module FSharpx.Tests.MapTests

open System
open FSharpx.Collections
open NUnit.Framework
open FsUnit

let data = 
        [
            (1,1); (2,2); (3,3); (4,4); (5,5)
            (6,6); (7,7); (8,8); (9,9); (10,10)
        ] |> Map.ofList

[<Test>]
let ``I should be able to choose elements from a map``() = 
    let expected = 
        [(2,2);(4,4);(6,6);(8,8);(10,10)]
        |> Map.ofList

    let actual = Map.choose (fun k _ -> if k % 2 = 0 then Some k else None) data

    actual |> should equal expected
    
[<Test>]
let ``I should be able to remove elements by key``() =
    let toRemove = [1;2;3;4;5]

    let actual = Map.removeMany toRemove data
    let expected = [ (6,6); (7,7); (8,8); (9,9); (10,10) ] |> Map.ofList
    actual |> should equal expected

[<Test>]
let ``I should be able to extract values from a map``() = 
    let expected = [1;2;3;4;5;6;7;8;9;10] |> Seq.ofList
    Map.values data |> should equal expected

[<Test>]
let ``I should be able to extract keys from a map``() = 
    let expected = [1;2;3;4;5;6;7;8;9;10] |> Seq.ofList
    Map.keys data |> should equal expected