// ----------------------------------------------------------------------------
// F# async extensions
// (c) Tomas Petricek, David Thomas 2012, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharp.Control
open System
open System.Threading

// ----------------------------------------------------------------------------

[<AutoOpen>]
module AsyncExtensions = 
    type Microsoft.FSharp.Control.Async with 

      /// Creates an asynchronous workflow that runs the asynchronous workflow
      /// given as an argument at most once. When the returned workflow is 
      /// started for the second time, it reuses the result of the 
      /// previous execution.
      static member Cache (input:Async<'T>) = 
          let agent = Agent<AsyncReplyChannel<_>>.Start(fun agent -> async {
              let! repl = agent.Receive()
              let! res = input
              repl.Reply(res)
              while true do
                  let! repl = agent.Receive()
                  repl.Reply(res) })

          async { return! agent.PostAndAsyncReply(id) }

      /// Starts the specified operation using a new CancellationToken and returns
      /// IDisposable object that cancels the computation. This method can be used
      /// when implementing the Subscribe method of IObservable interface.
      static member StartDisposable(op:Async<unit>) =
          let ct = new System.Threading.CancellationTokenSource()
          Async.Start(op, ct.Token)
          { new IDisposable with 
              member x.Dispose() = ct.Cancel() }

#if NET40

      /// Starts a non-generic Task with the cancellationToken and returns an
      /// Async<unit> containing the result.
      static member AwaitTask(task:Tasks.Task, ?cancellationToken) =
          let cancel = defaultArg cancellationToken Async.DefaultCancellationToken
          Async.AwaitTask <| task.ContinueWith((fun t -> ()), cancel)

      /// Starts a Task<'a> with the timeout and cancellationToken and
      /// returns a Async<a' option> containing the result.  If the Task does
      /// not complete in the timeout interval, or is faulted None is returned.
      static member TryAwaitTask(task:Tasks.Task<_>, ?timeout, ?cancellationToken) =
          let timeout = defaultArg timeout Timeout.Infinite
          let cancel = defaultArg cancellationToken Async.DefaultCancellationToken
          async {
              return
                if task.Wait(timeout, cancel) && not task.IsCanceled && not task.IsFaulted
                then Some task.Result
                else None }

    /// Implements an extension method that overloads the standard
    /// 'Bind' of the 'async' builder. The new overload awaits on 
    /// a standard .NET task
    type Microsoft.FSharp.Control.AsyncBuilder with
        member x.Bind(t:Tasks.Task<'T>, f:'T -> Async<'R>) : Async<'R> = async.Bind(Async.AwaitTask t, f)
        member x.Bind(t:Tasks.Task, f:unit -> Async<'R>)   : Async<'R> = async.Bind(Async.AwaitTask t, f)

#endif