module FSharpx.Tests.ArrayTests

open FSharpx
open NUnit.Framework
open FsUnit

[<Test>]
let ``I should be able to part of an array to a target array``() =
    let a, b = [|1;2;3;4;5|], [|10;11;12;13;14|]
    (Array.copyTo 0 2 a b) 
    b |> should equal [|10;11;1;2;3|]

[<Test>]
let ``I should be able to convert a tuple to an array``() = 
    (1,2) |> Array.ofTuple
    |> should equal [|1;2|]

[<Test>]
let ``I should be able to convert an array to a tuple``() = 
    let result : (int*int) = [|1;2|] |> Array.toTuple
    result |> should equal (1,2)