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

[<Test>]
let ``valid cast``() =
    let a = box 22
    let r = Choice.cast a
    Assert.AreEqual(22, Choice.get r)

[<Test>]
let ``invalid cast``() =
    let a = box "something"
    let r : Choice<int,exn> = Choice.cast a
    match r with
    | Choice1Of2 _ -> Assert.Fail("Cast should not have succeeded")
    | Choice2Of2 e -> 
        printfn "%A" e
        Assert.IsInstanceOf<InvalidCastException>(e)
    