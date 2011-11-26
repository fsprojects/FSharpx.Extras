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

[<Test>]
let ChoiceFolding() =
    // http://stackoverflow.com/questions/8249744/exception-handling-in-pipeline-sequence
    let startingPosition = 0. ,0.

    let moveByLengthAndAngle l a (x,y) = x,y // too lazy to do the math
    let moveByXandY dx dy (x,y) = 
        //failwith "oops"
        x+dx, y+dy
    let moveByXandAngle dx a (x,y) = x+dx, y

    let actions = 
        [
            moveByLengthAndAngle 0. 0., "failed first moveByLengthAndAngle"
            moveByXandY 1. 2., "failed moveByXandY"
            moveByXandY 3. 4., "failed moveByXandY"
            moveByXandAngle 3. 4., "failed moveByXandAngle"
            moveByLengthAndAngle 4. 5., "failed second moveByLengthAndAngle"
        ]

    let finalPosition = 
        let inline folder a (f,message) = 
            Choice.protect f a |> Choice.mapSecond (konst message)
        actions |> Choice.fold folder startingPosition

    match finalPosition with
    | Validation.Success (x,y) -> 
        printfn "final position: %f,%f" x y
    | Validation.Failure error -> 
        printfn "error: %s" error
        Assert.Fail("should not have failed: {0}", error)
    