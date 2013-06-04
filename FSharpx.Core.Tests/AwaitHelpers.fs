namespace FSharpx.Core.Tests

open System
open System.Threading
open System.Threading.Tasks

open FsUnit
open NUnit.Framework

type AwaiterResult<'a> =
    | Timeout
    | Canceled
    | Result of 'a
    | Error of exn
    with 
    override this.ToString() = 
        match this with
        | Timeout -> "Timeout"
        | Canceled -> "Canceled"
        | Result a -> a.ToString()
        | Error err -> err.ToString()

type Awaiter<'a> = TimeSpan -> 'a AwaiterResult

[<AutoOpen>]
module private AwaitHelpers =
    
    let startAsAwaiterWithCancellation (wf : Async<'a>, ct : CancellationToken option) : Awaiter<'a> =
        let gotCanceled = new ManualResetEventSlim(false)
        let withCancel = Async.TryCancelled(wf, fun _ -> gotCanceled.Set())
        let task = 
            match ct with 
            | Some ct -> Async.StartAsTask (withCancel, cancellationToken = ct)
            | None    -> Async.StartAsTask withCancel
        let awaiter = fun (timeout : TimeSpan) ->
            try
                let completed = task.Wait(timeout)
                if not completed && not gotCanceled.IsSet
                    then Timeout 
                elif task.IsCanceled || gotCanceled.IsSet
                    then Canceled 
                elif task.IsFaulted then 
                    match task.Exception.InnerException with
                    | :? TaskCanceledException -> Canceled
                    | _                        -> Error task.Exception.InnerException
                else
                    Result task.Result
            with
            | :? AggregateException as aEx -> 
                match aEx.InnerException with
                | :? TaskCanceledException -> Canceled
                | _                        -> Error aEx.InnerException
        awaiter

    let startAsAwaiter wf = startAsAwaiterWithCancellation (wf, None)