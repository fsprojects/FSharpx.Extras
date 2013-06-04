namespace FSharpx.Core.Tests

open System
open System.Threading
open System.Threading.Tasks

open FSharp.Control.Observable

open FsUnit
open NUnit.Framework

[<TestFixture>]
type ``AwaitObservable Tests``() = 

    [<Test>]
    member test.``AwaitObservable yields a value from the sources Next``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromMilliseconds(100.0))
        source.Next("DONE")
        source.Completed()
        let result = awaiter(TimeSpan.FromMilliseconds(100.0))
        result |> should equal (Result "DONE")

    [<Test>]
    member test.``AwaitObservable yields the first value from the sources Next``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromMilliseconds(100.0))
        source.Next("ONE")
        source.Next("TWO")
        source.Completed()
        let result = awaiter(TimeSpan.FromMilliseconds(100.0))
        result |> should equal (Result "ONE")

    [<Test>]
    member test.``AwaitObservable is canceled if the source completes without a single result``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromMilliseconds(100.0))
        source.Completed()
        let result = awaiter(TimeSpan.FromMilliseconds(100.0))
        result |> should equal AwaiterResult<string>.Canceled    
        
    [<Test>]
    member test.``AwaitObservable is unsubscribed from the source after a value was received``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromMilliseconds(100.0))
        source.Next("Done")
        source.AssertUnsubscribe(TimeSpan.FromMilliseconds(100.0))

    [<Test>]
    member test.``AwaitObservable is unsubscribed from the source after the source completes without a result``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromMilliseconds(100.0))
        source.Completed()
        source.AssertUnsubscribe(TimeSpan.FromMilliseconds(100.0))

    [<Test>]
    member test.``AwaitObservable is unsubscribed from the source after OnError was called``() =
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiter wf
        source.AssertSubscribtion(TimeSpan.FromMilliseconds(100.0))
        source.Error(exn "test-error")
        source.AssertUnsubscribe(TimeSpan.FromMilliseconds(100.0))

    [<Test>]
    member test.``AwaitObservable is unsubscribed from the source if it's resulting async-workflow gets cancelled``() =
        let cts = new CancellationTokenSource()
        let source = new ObservableMock<string>()
        let wf = Async.AwaitObservable source
        let awaiter = startAsAwaiterWithCancellation (wf, Some cts.Token)
        source.AssertSubscribtion(TimeSpan.FromMilliseconds(100.0))
        cts.Cancel()
        let result = awaiter (TimeSpan.FromMilliseconds(100.0)) 
        result |> should equal AwaiterResult<string>.Canceled
        source.AssertUnsubscribe(TimeSpan.FromMilliseconds(100.0))
        