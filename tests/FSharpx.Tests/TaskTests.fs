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
        //Task.Factory.FromAsync((fun a b -> x.BeginGetResponse(a,b)), x.EndGetResponse, null)
        x.AsyncGetResponse() |> Async.toTask

type StreamReader with
    member x.ReadToEndAsync() = 
        x.AsyncReadToEnd() |> Async.toTask

[<Test>]
let loadprices() =
    let url = "http://ichart.finance.yahoo.com/table.csv?s=MSFT&d=9&e=30&f=2008&g=d&a=2&b=13&c=1986&ignore=.csv"
    let req = WebRequest.Create url
    let downloadTask =
        task {
            let! resp = req.GetResponseAsync()
            use stream = resp.GetResponseStream()
            use reader = new StreamReader(stream)
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
    let t,p = downloadTask.Result.[0]
    Assert.AreEqual(DateTime(2008,10,30), t)
    Assert.AreEqual(20.82m, p)

#endif