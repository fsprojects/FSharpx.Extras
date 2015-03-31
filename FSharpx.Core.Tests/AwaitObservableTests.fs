namespace FSharpx.Control.Tests

open System
open System.Threading
open System.Threading.Tasks

open FsUnit
open NUnit.Framework
open FSharpx.Control
open FSharpx.Control.AsyncExtensions
open FSharpx.Control.StreamReaderExtensions
open FSharpx.Control.Observable

[<TestFixture>]
type ``AwaitObservable Tests``() = 

    [<Test; Ignore("Failing on appveyor ('A continuation provided by Async.FromContinuations was invoked multiple times')") >]
    member test.``AwaitObservable yields a value from the sources Next``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Next("DONE")
        source.Completed()
        let result = awaiter(TimeSpan.FromSeconds(1.0))
        result |> should equal (Result "DONE")

    [<Test; Ignore("Failing on appveyor ('A continuation provided by Async.FromContinuations was invoked multiple times')") >]
    member test.``AwaitObservable yields the first value from the sources Next``() =
        let source = new ObservableMock<string>()
        let wf = Async .AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Next("ONE")
        source.Next("TWO")
        source.Completed()
        let result = awaiter(TimeSpan.FromSeconds(1.0))
        result |> should equal (Result "ONE")

    [<Test ; Ignore("Failing on appveyor ('a observer-subscription was not disposed')")  >]
    member test.``AwaitObservable is canceled if the source completes without a single result``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Completed()
        let result = awaiter(TimeSpan.FromSeconds(1.0))
        result |> should equal AwaiterResult<string>.Canceled    
        
    [<Test ; Ignore("Failing on appveyor ('a observer-subscription was not disposed')")  >]
    member test.``AwaitObservable is unsubscribed from the source after a value was received``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Next("Done")
        source.AssertUnsubscribe(TimeSpan.FromSeconds(1.0))

    [<Test ; Ignore("Failing on appveyor ('a observer-subscription was not disposed')")  >]
    member test.``AwaitObservable is unsubscribed from the source after the source completes without a result``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Completed()
        source.AssertUnsubscribe(TimeSpan.FromSeconds(1.0))

    [<Test ; Ignore("Failing on appveyor ('a observer-subscription was not disposed')")  >]
    member test.``AwaitObservable is unsubscribed from the source after OnError was called``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        source.Error(exn "test-error")
        source.AssertUnsubscribe(TimeSpan.FromSeconds(1.0))

    [<Test ; Ignore("Failing on appveyor ('a observer-subscription was not disposed')")  >]
    member test.``AwaitObservable is unsubscribed from the source if it's resulting async-workflow gets cancelled``() =
        let cts = new CancellationTokenSource()
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiterWithCancellation (wf, Some cts.Token)
        source.AssertSubscribtion(TimeSpan.FromSeconds(1.0))
        cts.Cancel()
        let result = awaiter (TimeSpan.FromSeconds(1.0)) 
        result |> should equal AwaiterResult<string>.Canceled
        source.AssertUnsubscribe(TimeSpan.FromSeconds(1.0))
        