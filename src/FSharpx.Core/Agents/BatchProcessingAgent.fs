// ----------------------------------------------------------------------------
// F# async extensions (BatchProcessingAgent.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharpx.Control

open System
open System.Threading

// ----------------------------------------------------------------------------

/// Agent that can be used to implement batch processing. It creates groups
/// of messages (added using the Enqueue method) and emits them using the 
/// BatchProduced event. A group is produced when it reaches the maximal 
/// size or after the timeout elapses.
type BatchProcessingAgent<'T>(batchSize, timeout) = 

    let batchEvent = new Event<'T[]>()

    let cts = new CancellationTokenSource()

    let body (agent: Agent<'T>) =
        let rec loop remainingTime messages = async {
            let start = DateTime.Now
            let! msg = agent.TryReceive(timeout = max 0 remainingTime)
            let elapsed = int (DateTime.Now - start).TotalMilliseconds
            match msg with 
            | Some(msg) when List.length messages = batchSize - 1 ->
                batchEvent.Trigger(msg :: messages |> List.rev |> Array.ofList)
                return! loop timeout []
            | Some(msg) ->
                return! loop (remainingTime - elapsed) (msg::messages)
            | None when List.length messages <> 0 -> 
                batchEvent.Trigger(messages |> List.rev |> Array.ofList)
                return! loop timeout []
            | None -> 
                return! loop timeout [] }
        loop timeout []
    let agent : Agent<'T> = Agent.Start(body, cts.Token)

    /// The event is triggered when a group of messages is collected. The
    /// group is not empty, but may not be of the specified maximal size
    /// (when the timeout elapses before enough messages is collected)
    [<CLIEvent>]
    member x.BatchProduced = batchEvent.Publish

    /// Sends new message to the agent
    member x.Enqueue v = agent.Post(v)

    /// Dispose
    interface IDisposable with
        member x.Dispose() = cts.Cancel()
