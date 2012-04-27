module FSharpx.Tests.TaskTests
#if NET40

open System
open System.IO
open System.Net
open NUnit.Framework
open FSharpx
open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Control.WebExtensions

let task = Task.TaskBuilder(continuationOptions = TaskContinuationOptions.ExecuteSynchronously)

type StreamReader with
    member x.ReadToEndAsync() = 
        x.AsyncReadToEnd() |> Async.StartAsTask

type File with
    static member OpenTextAsync path =
        File.AsyncOpenText path |> Async.StartAsTask

let filename = @"..\..\table.csv"

[<Test>]
let loadprices() =
    let started = ref false
    let processTask =
        task {
            started := true
            use! reader = File.OpenTextAsync filename
            let! csv = reader.ReadToEndAsync()
            let prices =
                csv.Split([|'\n'|])
                |> Seq.skip 1
                |> Seq.map (fun line -> line.Split([|','|]))
                |> Seq.filter (fun values -> values |> Seq.length = 7)
                |> Seq.map (fun values ->
                    let t = DateTime.parse values.[0] |> Option.get
                    let p = decimal values.[6]
                    t,p)
                |> Seq.toList
            return prices
        }
    Assert.False(!started)
    let t,p = processTask().Result.[0]
    Assert.AreEqual(DateTime(2008,10,30), t)
    Assert.AreEqual(20.82m, p)
    Assert.True(!started)

[<Test>]
let ``exception in task``() =
    let t = 
        task {
            failwith "error"
        }
    match Task.run t with
    | Task.Error e -> Assert.AreEqual("error", e.Message)
    | _ -> Assert.Fail "task should have errored"

[<Test>]
let ``canceled task``() =
    use cts = new CancellationTokenSource()
    let task = Task.TaskBuilder(cancellationToken = cts.Token)
    let t = 
        task {
            cts.Token.ThrowIfCancellationRequested()
        }
    cts.Cancel()
    match Task.run t with
    | Task.Canceled -> ()
    | Task.Error e -> Assert.Fail("Task should have been canceled, but errored with exception {0}", e)
    | Task.Successful a -> Assert.Fail("Task should have been canceled, but succeeded with result {0}", a)

[<Test>]
let ``canceled task 2``() =
    use cts = new CancellationTokenSource()
    let task = Task.TaskBuilder(cancellationToken = cts.Token)
    let t = 
        task {
            use! reader = File.OpenTextAsync filename // just something to do a bind
            return ()
        }
    cts.Cancel()
    match Task.run t with
    | Task.Canceled -> ()
    | Task.Error e -> Assert.Fail("Task should have been canceled, but errored with exception {0}", e)
    | Task.Successful a -> Assert.Fail("Task should have been canceled, but succeeded with result {0}", a)

#endif