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

#endif

    /// Creates an asynchronous workflow that non-deterministically returns the 
    /// result of one of the two specified workflows (the one that completes
    /// first). This is similar to Task.WhenAny.
    static member WhenAny([<ParamArray>] works:Async<'T>[]) : Async<'T> =
      Async.FromContinuations(fun (cont, econt, ccont) ->
        // Results from the two 
        let results = Array.map (fun _ -> Choice1Of3()) works
        let handled = ref false
        let lockObj = new obj()
        let synchronized f = lock lockObj f

        // Called when one of the workflows completes
        let complete () = 
          let op =
            synchronized (fun () ->
              // If we already handled result (and called continuation)
              // then ignore. Otherwise, if the computation succeeds, then
              // run the continuation and mark state as handled.
              // Only throw if all workflows failed.
              if !handled then ignore
              else
                let succ = Seq.tryPick (function Choice2Of3 v -> Some v | _ -> None) results
                match succ with
                | Some value -> handled := true; (fun () -> cont value)
                | _ ->
                  if Seq.forall (function Choice3Of3 _ -> true | _ -> false) results then
                    let exs = Array.map (function Choice3Of3 ex -> ex | _ -> failwith "!") results
                    (fun () -> econt (AggregateException(exs)))
                  else ignore )
          // Actually run the continuation
          // (this shouldn't be done in the lock)
          op() 

        // Run a workflow and write result (or exception to a ref cell)
        let run index workflow = async {
          try
            let! res = workflow
            synchronized (fun () -> results.[index] <- Choice2Of3 res)
          with e ->
            synchronized (fun () -> results.[index] <- Choice3Of3 e)
          complete() }

        // Start all work items - using StartImmediate, because it
        // should be started on the current synchronization context
        works |> Seq.iteri (fun index work ->
          Async.StartImmediate(run index work)) )

  /// Extend the standard F# 'async' builder with 
  /// additional operations to support joinads
  type Microsoft.FSharp.Control.AsyncBuilder with

    /// Non-deterministically choose the first computation
    /// that succeeds; fails only when both computations fail
    member x.Choose(a, b) = Async.WhenAny(a, b)

    /// Represents a failed computation. It holds that:
    /// async.Choose(work, async.Fail()) = work
    member x.Fail<'T>() : Async<'T> = async { 
      return failwith "failed!" }

    /// Run the specified two computations in parallel and
    /// return their results as a tuple
    member x.Merge(a:Async<'T1>, b:Async<'T2>) : Async<'T1 * 'T2> = async {
      let map f v = async.Bind(v, f >> async.Return)
      let works = [ map box a; map box b ]
      let! res = Async.Parallel works
      return unbox res.[0], unbox res.[1] }

    /// Returns a computation that, when started, runs the workflow 
    /// given as an argument and returns a new computation that 
    /// can be used to wait for the result of the started workflow
    member x.Alias(a) = Async.StartChild(a)
