module FSharpx.Tests.TimeMeasurementTests

open FSharpx.TimeMeasurement
open NUnit.Framework
open System

[<Test>]
let ``When using a stopwatch the elapsed time should be measured``() =
    let f = fun () -> Threading.Thread.Sleep(100)
    let runtime = stopTime f |> snd

    Assert.GreaterOrEqual(runtime,100.)

[<Test>]
let ``When using a stopwatch the average time should be measured ``() =
    let f = fun () -> Threading.Thread.Sleep(100)
    let runtime = stopAverageTime 10 f |> snd

    Assert.GreaterOrEqual(runtime, 80.)