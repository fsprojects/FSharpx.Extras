// ----------------------------------------------------------------------------
// F# async extensions (AutoCancelAgent.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharpx.Control

open System
open System.Threading

// ----------------------------------------------------------------------------
// See also: http://fssnip.net/64

/// Wrapper for the standard F# agent (MailboxProcessor) that
/// supports stopping of the agent's body using the IDisposable 
/// interface (the type automatically creates a cancellation token)
type AutoCancelAgent<'T> private (mbox:Agent<'T>, cts:CancellationTokenSource) = 

  /// Start a new disposable agent using the specified body function
  /// (the method creates a new cancellation token for the agent)
  static member Start(f) = 
    let cts = new CancellationTokenSource()
    new AutoCancelAgent<'T>(Agent<'T>.Start(f, cancellationToken = cts.Token), cts)
  
  /// Returns the number of unprocessed messages in the message queue of the agent.
  member x.CurrentQueueLength = mbox.CurrentQueueLength

  /// Occurs when the execution of the agent results in an exception.
  [<CLIEvent>]
  member x.Error = mbox.Error

  /// Waits for a message. This will consume the first message in arrival order.
  member x.Receive(?timeout) = mbox.Receive(?timeout = timeout)

  /// Scans for a message by looking through messages in arrival order until <c>scanner</c> 
  /// returns a Some value. Other messages remain in the queue.
  member x.Scan(scanner, ?timeout) = mbox.Scan(scanner, ?timeout = timeout)

  /// Like PostAndReply, but returns None if no reply within the timeout period.
  member x.TryPostAndReply(buildMessage, ?timeout) = 
    mbox.TryPostAndReply(buildMessage, ?timeout = timeout)

  /// Waits for a message. This will consume the first message in arrival order.
  member x.TryReceive(?timeout) = 
    mbox.TryReceive(?timeout = timeout)

  /// Scans for a message by looking through messages in arrival order until <c>scanner</c> 
  /// returns a Some value. Other messages remain in the queue.
  member x.TryScan(scanner, ?timeout) = 
    mbox.TryScan(scanner, ?timeout = timeout)

  /// Posts a message to the message queue of the MailboxProcessor, asynchronously.
  member x.Post(m) = mbox.Post(m)

  /// Posts a message to an agent and await a reply on the channel, synchronously.
  member x.PostAndReply(buildMessage, ?timeout) = 
    mbox.PostAndReply(buildMessage, ?timeout = timeout)

  /// Like PostAndAsyncReply, but returns None if no reply within the timeout period.
  member x.PostAndTryAsyncReply(buildMessage, ?timeout) = 
    mbox.PostAndTryAsyncReply(buildMessage, ?timeout = timeout)

  /// Posts a message to an agent and await a reply on the channel, asynchronously.
  member x.PostAndAsyncReply(buildMessage, ?timeout) = 
    mbox.PostAndAsyncReply(buildMessage, ?timeout=timeout)

  interface IDisposable with
    member x.Dispose() = 
      (mbox :> IDisposable).Dispose()
      cts.Cancel()
