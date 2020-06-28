module FSharpx.Tests.ChoiceTests

open System
open NUnit.Framework
open FSharpx
open FSharpx.Functional
open FsUnit
open TestHelpers

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
        actions |> Choice.foldM folder startingPosition

    match finalPosition with
    | Validation.Success (x,y) ->
        printfn "final position: %f,%f" x y
    | Validation.Failure error ->
        printfn "error: %s" error
        Assert.Fail("should not have failed: {0}", error)

open FSharpx.Choice

[<Test>]
let ``computations are aborted on the first Choice2Of2``() =
    let success = Choice1Of2 1
    let failure : Choice<int,string> = Choice2Of2 "failed computation"
    choose {
        let! x = success
        let! y = failure
        failwith "should never be called"
        return x + y }
    |> should equal failure

[<Test>]
let ``multiple successful values propagate through``() =
    let a = Choice1Of2 1
    let b = Choice1Of2 2
    choose {
        let! x = a
        let! y = b
        return x + y }
    |> should equal (Choice1Of2 3)

[<Test>]
let ``return! allows binding the result``() =
    let f = Choice1Of2
    choose { return! f 6 }
    |> should equal (Choice1Of2 6)

open FsCheck
open FsCheck.NUnit

[<Test>]
let ``monad laws``() =
    let ret (x: int) = choose.Return x
    let n = sprintf "Choice : monad %s"
    let inline (>>=) m f = choose.Bind(m,f)
    fsCheck "left identity" <|
        fun f a -> ret a >>= f = f a
    fsCheck "right identity" <|
        fun x -> x >>= ret = x
    fsCheck "associativity" <|
        fun f g v ->
            let a = (v >>= f) >>= g
            let b = v >>= (fun x -> f x >>= g)
            a = b

[<Test>]
let ``use should dispose underlying IDisposable on Choice1Of2``() =
    let disposeChecker = new DisposeChecker()
    let r = choose {
        use! x = Choice1Of2 disposeChecker
        return x.Disposed
    }
    Assert.Multiple
      (fun () ->
          disposeChecker.Disposed |> shouldEqual true
          r |> shouldEqual (Choice1Of2 false)
      )

[<Test>]
let ``use should dispose underlying IDisposable on Choice2Of2``() =
    let disposeChecker = new DisposeChecker()
    let r = choose {
        use! x = Choice1Of2 disposeChecker
        let! y = Choice2Of2 "error"
        return x.Disposed
    }
    Assert.Multiple
      (fun () ->
        disposeChecker.Disposed |> shouldEqual true
        r |> shouldEqual (Choice2Of2 "error")
      )
