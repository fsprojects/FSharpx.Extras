// ----------------------------------------------------------------------------
// F# async extensions (BatchProcessingAgent.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharp.Control

open System
open System.Threading

// ----------------------------------------------------------------------------

/// Agent that can be used to implement batch processing. It creates groups
/// of messages (added using the Enqueue method) and emits them using the 
/// BatchProduced event. A group is produced when it reaches the maximal 
/// size or after the timeout elapses.
type BatchProcessingAgent<'T>(batchSize,timeout) = 

    let batchEvent = new Event<'T[]>()

    let cts = new CancellationTokenSource()
    
    let newSegment () =
        let array : 'T array = Array.zeroCreate batchSize
        new ArraySegment<_>(array, 0, 0)

    let segmentToArray (segment:ArraySegment<_>) =
        let array = Array.zeroCreate segment.Count
        Array.Copy(segment.Array, array, segment.Count)
        array

    let expandSegment item (segment:ArraySegment<_>) =
        segment.Array.[segment.Count] <- item
        new ArraySegment<_>(segment.Array, segment.Offset, segment.Count + 1)

    let body (agent:Agent<_>) =
        
        let rec loop remainingTime (messages:ArraySegment<_>) = async {
            let start = DateTime.Now
            let! msg = agent.TryReceive(timeout = max 0 remainingTime)
            let elapsed = int (DateTime.Now - start).TotalMilliseconds
            match msg with 
            | Some(msg) when messages.Count = batchSize - 1 ->
                batchEvent.Trigger(messages |> expandSegment msg |> segmentToArray)
                return! loop timeout (newSegment())
            | Some(msg) ->
                return! loop (remainingTime - elapsed) (messages |> expandSegment msg)
            | None when messages.Count <> 0 -> 
                batchEvent.Trigger(messages |> segmentToArray)
                return! loop timeout (newSegment())
            | None -> 
                return! loop timeout (newSegment()) }

        loop timeout (newSegment())

    let agent = Agent<_>.Start(body, cts.Token)

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
