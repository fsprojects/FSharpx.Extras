module FSharpx.Tests.TaskTests

open System
open System.IO
open System.Net
open NUnit.Framework
open FSharpx
open System.Threading

#if NET40
open System.Threading.Tasks
open Microsoft.FSharp.Control.FileExtensions
open Microsoft.FSharp.Control.StreamReaderExtensions
open Microsoft.FSharp.Control.WebExtensions

type StreamReader with
    member x.ReadToEndAsync() = 
        x.AsyncReadToEnd() |> Async.StartAsTask

type File with
    static member OpenTextAsync path =
        File.AsyncOpenText path |> Async.StartAsTask

let filename = @"table.csv"

//[<Test>]
//let loadprices() =    
//    let task = Task.TaskBuilder(continuationOptions = TaskContinuationOptions.ExecuteSynchronously)
//    let started = ref false
//    let dummy = ref false
//    let processTask() =
//        task {
//            started := true
//            use! reader = File.OpenTextAsync filename
//            let! csv = reader.ReadToEndAsync()
//            if !started 
//                then dummy := true
//            let prices =
//                csv.Split([|'\n'|])
//                |> Seq.skip 1
//                |> Seq.map (fun line -> line.Split([|','|]))
//                |> Seq.filter (fun values -> values |> Seq.length = 7)
//                |> Seq.map (fun values ->
//                    let t = DateTime.parse values.[0] |> Option.get
//                    let p = decimal values.[6]
//                    t,p)
//                |> Seq.toList
//            return prices
//        }
//    Assert.False !started
//    let t,p = processTask().Result.[0]
//    Assert.True !dummy
//    Assert.AreEqual(DateTime(2008,10,30), t)
//    Assert.AreEqual(20.82m, p)
//    Assert.True !started

[<Test>]
let ``task should return the right value after let!``() =
    let task = Task.TaskBuilder()
    let t() = 
        task {
            let! v = Task.Factory.StartNew(fun () -> 100)
            return v
        }

    match Task.run t with
    | Task.Canceled -> Assert.Fail("Task should have been successful, but was canceled")
    | Task.Error e -> Assert.Fail("Task should have been successful, but errored with exception {0}", e)
    | Task.Successful a -> Assert.AreEqual(100,a)


[<Test>]
let ``task should return the right value after return!``() =
    let task = Task.TaskBuilder()
    let t() = 
        task {
            return! Task.Factory.StartNew(fun () -> "hello world")
        }

    match Task.run t with
    | Task.Canceled -> Assert.Fail("Task should have been successful, but was canceled")
    | Task.Error e -> Assert.Fail("Task should have been successful, but errored with exception {0}", e)
    | Task.Successful a -> Assert.AreEqual("hello world",a)


[<Test>]
let ``exception in task``() =    
    let task = Task.TaskBuilder(continuationOptions = TaskContinuationOptions.ExecuteSynchronously)
    let t() = 
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
    let t() = 
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
    let t() = 
        task {
            let! v = Task.Factory.StartNew(fun () -> 0)
            return ()
        }
    cts.Cancel()
    match Task.run t with
    | Task.Canceled -> ()
    | Task.Error e -> Assert.Fail("Task should have been canceled, but errored with exception {0}", e)
    | Task.Successful a -> Assert.Fail("Task should have been canceled, but succeeded with result {0}", a)

[<Test>]
let ``while``() = 
    let i = ref 10    
    let task = Task.TaskBuilder(continuationOptions = TaskContinuationOptions.ExecuteSynchronously)
    let t() =
        task {
            while !i > 0 do
                decr i
                do! Task.Factory.StartNew ignore
        }
    Task.run t |> ignore
    Assert.AreEqual(0, !i)

open FsCheck
open FsCheck.NUnit

type TaskGen =
    static member TaskArb =
        Arb.generate |> Gen.map Task.returnM |> Arb.fromGen

[<Test>]
let ``run delay law``() =
    Arb.register<TaskGen>() |> ignore
    let task = Task.TaskBuilder(continuationOptions = TaskContinuationOptions.ExecuteSynchronously)
    fsCheck "run delay law" (fun a -> (task.Run << task.Delay << konst) a = a)

#endif
