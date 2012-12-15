module FSharpx.Tests.ArrayTests

open FSharpx.Collections
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

let data = [|1.;2.;3.;4.;5.;6.;7.;8.;9.;10.|]

[<Test>]
let ``I should be able to create a centered window from a seq``() =
    let result = Array.centeredWindow 3 data
    result |> should equal [|
                                [|1;2;3;4|]
                                [|1;2;3;4;5|]
                                [|1;2;3;4;5;6|]
                                [|1;2;3;4;5;6;7|]
                                [|2;3;4;5;6;7;8|]
                                [|3;4;5;6;7;8;9|]
                                [|4;5;6;7;8;9;10|]
                                [|5;6;7;8;9;10|]
                                [|6;7;8;9;10|]
                                [|7;8;9;10|]
                            |]

[<Test>]
let ``I should be able to compute the central moving average of a seq``() =
    let result = Array.centralMovingAverage 3 data |> Seq.toList
    result |> should equal [|
                                2.5; 3.; 3.5; 4.; 
                                5.; 6.; 7.;
                                7.5; 8.; 8.5
                           |]