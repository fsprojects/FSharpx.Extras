module FSharpx.Collections.Experimental.Tests.TimeSeriesTest

open System
open FSharpx.Collections.Experimental
open NUnit.Framework
open FsUnit

let startDate = (new DateTimeOffset(2012, 1, 1, 0, 0, 0, TimeSpan.Zero))
let size = 72
let granularity = TimeSpan.FromHours(1.)

[<Test>]
let ``I should be able to extract the values from the timeseries at a smaller granularity``() = 
    let actual = (new Timeseries<int>(startDate, granularity, size)).ToGranularity(TimeSpan.FromMinutes(5.)).AsTimeseries() |> Seq.toList
    let expected = Seq.init (72 * 12) (fun i -> startDate.AddMinutes((i |> float) * 5.), 0) |> Seq.toList
    actual |> should equal expected

[<Test>]
let ``I should be able to extract the values from the timeseries at a larger granularity``() = 
    let actual = (new Timeseries<int>(startDate, TimeSpan.FromMinutes(5.), 864)).ToGranularity(TimeSpan.FromHours(1.)).AsTimeseries() |> Seq.toList
    let expected = Seq.init 72 (fun i -> startDate.AddMinutes((i |> float) * 60.), 0) |> Seq.toList
    actual |> should equal expected

[<Test>]
let ``I can create a timeseries``() = 
    let actual = new Timeseries<int>(startDate, granularity, size)
    let expected = Seq.init 72 (fun _ -> 0)
    actual.Buffer.ToArray() |> should equal expected
    
[<Test>]
let ``I should not be able to advance a timeseries less than one minute``() =
    let toDate = startDate.AddSeconds(30.)
    let actual = new Timeseries<int>(startDate, granularity, size)
    let startDate' = actual.Advance(toDate)
    startDate' |> should equal startDate

[<Test>]
let ``I should not be able to advance a timeseries to a past date``() =
    let toDate = startDate.AddDays(-1.)
    let actual = new Timeseries<int>(startDate, granularity, size)
    Assert.Throws<ArgumentException>(fun _ -> actual.Advance(toDate) |> ignore) |> ignore

[<Test>]
let ``I should be to advance a timeseries to a future date``() =
    let toDate = startDate.AddDays(1.)
    let actual = new Timeseries<int>(startDate, granularity, Seq.init 72 id)
    let expected = 
        seq {
                yield! Seq.init 48 (fun i -> i + 24)
                yield! Seq.init 24 (fun _ -> 0)
            }
    actual.Advance(toDate) |> ignore
    actual.StartDate |> should equal toDate
    actual.Buffer.Position |> should equal 0
    actual.Buffer.ToArray() |> should equal expected 

[<Test>]
let ``I should be able to insert a set of values with the different date and same granularity``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun a b -> b), startDate.AddDays(1.), granularity, List.init 10 (fun i -> i + 1))
    let expected =
        seq {
                yield! Array.zeroCreate 24
                yield! [1..10]
                yield! Array.zeroCreate 38
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should be able to insert a set of values with the different date and smaller granularity``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun a b -> b), startDate.AddDays(1.), TimeSpan.FromMinutes(12.), List.init 10 (fun i -> i + 1))
    let expected =
        seq {
                yield! Array.zeroCreate 24
                yield! [5;10]
                yield! Array.zeroCreate 46
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should be able to insert a set of values with the different date and larger granularity``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun a b -> b), startDate.AddDays(1.), TimeSpan.FromHours(2.), List.init 10 (fun i -> i + 1))
    let expected =
        seq {
                yield! Array.zeroCreate 24
                yield! (List.init 10 (fun i -> [i + 1;i+1])) |> List.concat
                yield! Array.zeroCreate 28
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should be able to insert a set of values with the same date and granularity``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun a b -> b), startDate, granularity, List.init 10 (fun i -> i + 1))
    let expected =
        seq {
                yield! [1..10]
                yield! Array.zeroCreate 62
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should be able to insert a set of values with the same date and smaller granularity``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun a b -> b), startDate, TimeSpan.FromMinutes(12.), List.init 10 (fun i -> i + 1))
    let expected =
        seq {
                yield! [5;10]
                yield! Array.zeroCreate 70
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should be able to insert a set of values with the same date and larger granularity``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun a b -> b), startDate, TimeSpan.FromHours(2.), List.init 10 (fun i -> i + 1))
    let expected =
        seq {
                yield! (List.init 10 (fun i -> [i + 1;i+1])) |> List.concat
                yield! Array.zeroCreate 52
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should be able to insert a set of values with the past date and same granularity``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun a b -> b), startDate.AddDays(-1.), granularity, List.init 48 (fun i -> i + 1))
    let expected =
        seq {
                yield! [25..48]
                yield! Array.zeroCreate 48
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should be able to insert a set of values with the past date and smaller granularity``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun a b -> b), startDate.AddDays(-1.), TimeSpan.FromMinutes(12.), List.init 288 (fun i -> i + 1))
    let expected =
        seq {
                yield! [125..5..285]
                yield! Array.zeroCreate 39
            } |> Seq.toList
    actual.Buffer.ToArray() |> Seq.toList |> should equal expected

[<Test>]
let ``I should be able to insert a set of values with the past date and larger granularity``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun a b -> b), startDate.AddDays(-1.), TimeSpan.FromHours(2.), List.init 24 (fun i -> i + 1))
    let expected =
        seq {
                yield! (List.init 12 (fun i -> [i + 13; i + 13])) |> List.concat
                yield! Array.zeroCreate 48
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should be able to insert a set of values offset from the timeseries start without wrapping round beyond the timeseries position``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun _ b -> b), startDate, granularity, List.init size (fun i -> 1))
    actual.Insert((fun _ b -> b), startDate.AddDays(1.), granularity, List.init (size * 2) (fun i -> 2)) 
    let expected =
        seq {
                yield! Array.init 24 (fun i -> 1)
                yield! Array.init 48 (fun i -> 2)
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should not be able to insert a set of values offset beyond the timeseries position``() =
    let actual = new Timeseries<int>(startDate, granularity, size)
    actual.Insert((fun _ b -> b), startDate, granularity, List.init size (fun i -> 1))
    actual.Insert((fun _ b -> b), startDate.AddDays(4.), granularity, List.init (size * 2) (fun i -> 2)) 
    let expected =
        seq {
                yield! Array.init size (fun i -> 1)                   
            }
    actual.Buffer.ToArray() |> should equal expected

[<Test>]
let ``I should be able to clone a timeseries``() =
    let buffer = new Timeseries<int>(startDate, granularity, [|1;2;3;4;5;6;7;8;9;10|])
    let clone = buffer.Clone()
    buffer.Buffer.ToArray() |> should equal (clone.Buffer.ToArray())
    clone.Buffer.Advance(2) |> ignore
    Assert.AreNotEqual(buffer.Buffer.ToArray(),clone.Buffer.ToArray())
    buffer.Buffer.Position |> should equal 0
    clone.Buffer.Position |> should equal 2
       
[<Test>]
let ``I should be able to create an empty timeseries in the past and extract all values``() =
    let actual = new Timeseries<Nullable<float>>(startDate, granularity, 10)
    let expected = 
        Seq.init 10 (fun i -> startDate.AddHours(i |> float), Nullable<float>())
    actual.AsTimeseries(startDate) |> should equal expected    
       
[<Test>]
let ``I should be able to insert a set of values with a zero offset from the timeseries start applying an operation between existing and new values``() =
    let merge op (a:Nullable<float>) (b:Nullable<float>) =
        if a.HasValue then
            if b.HasValue then Nullable(op a.Value b.Value) else a
        else
            b
    let actual = new Timeseries<Nullable<float>>(startDate, granularity, 10)
    actual.Insert(merge (+), startDate, granularity, [1..10] |> List.map (fun i -> Nullable(float i)))
    actual.Insert(merge (+), startDate, granularity, [1..10] |> List.map (fun i -> Nullable(float i)))
    let expected = [1..10] |> List.map (fun i -> startDate.AddHours(i - 1 |> float), Nullable<float>(float i * 2.))
    actual.AsTimeseries(startDate) |> should equal expected           