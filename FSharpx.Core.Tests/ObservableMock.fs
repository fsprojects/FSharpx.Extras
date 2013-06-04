namespace FSharpx.Core.Tests

open System
open System.Threading
open NUnit.Framework

type ObservableMock<'a>() =

    let _waitForSubscribtion = new ManualResetEventSlim(initialState = false)
    let _waitForUnsubscribe = new ManualResetEventSlim(initialState = false)
    let _connectedObserver : 'a IObserver option ref = ref None

    let callObserver action =
        match !_connectedObserver with
        | Some obs -> action obs
        | None     -> ()

    member this.WaitForSubscribtionHandle = _waitForSubscribtion.WaitHandle
    member this.WaitForSubscribtion(timeout : TimeSpan) = _waitForSubscribtion.Wait(timeout)
    member this.AssertSubscribtion(timeout : TimeSpan) =
        if not <| this.WaitForSubscribtion(timeout) then Assert.Fail("no subscribtion requested")

    member this.WaitForUnsubscribeHandle = _waitForUnsubscribe.WaitHandle
    member this.WaitForUnsubscribe(timeout : TimeSpan) = _waitForUnsubscribe.Wait(timeout)
    member this.AssertUnsubscribe(timeout : TimeSpan) =
        if not <| this.WaitForUnsubscribe(timeout) then Assert.Fail("a observer-subscription was not disposed")

    member this.Next(value : 'a) = callObserver (fun obs -> obs.OnNext(value))
    member this.Completed() = callObserver (fun obs -> obs.OnCompleted())
    member this.Error(error : exn) = callObserver (fun obs -> obs.OnError(error))

    interface IObservable<'a> with
        member i.Subscribe(observer : IObserver<'a>) =
            _connectedObserver := Some observer
            _waitForSubscribtion.Set()
            _waitForUnsubscribe.Reset()
            { new IDisposable with 
                member i.Dispose() = 
                    _connectedObserver := None 
                    _waitForUnsubscribe.Set()
            }
