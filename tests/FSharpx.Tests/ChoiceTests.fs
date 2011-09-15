module FSharpx.Tests.ChoiceTests

open System
open NUnit.Framework
open FSharpx
open FsUnit

[<Test>]
let getChoice1Of2() =
    let a = Choice.returnM 23
    Choice.get a |> should equal 23

[<Test>]
let getChoice2Of2() =
    let a = Choice2Of2 23
    (fun () -> Choice.get a |> ignore) |> should throw typeof<ArgumentException>