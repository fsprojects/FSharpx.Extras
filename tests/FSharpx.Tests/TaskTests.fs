module FSharpx.Tests.TaskTests
#if NET40

open System
open System.IO
open System.Net
open NUnit.Framework
open FSharpx
open System.Threading.Tasks
open Microsoft.FSharp.Control.WebExtensions

let task = Task.TaskBuilder(continuationOptions = TaskContinuationOptions.ExecuteSynchronously)

type WebRequest with
    member x.GetResponseAsync() =
        x.AsyncGetResponse() |> Async.toTask

type StreamReader with
    member x.ReadToEndAsync() = 
        x.AsyncReadToEnd() |> Async.toTask

type File with
    static member OpenTextAsync path =
        File.AsyncOpenText path |> Async.toTask

[<Test>]
let loadprices() =
    let filename = @"..\..\table.csv"
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

#endif