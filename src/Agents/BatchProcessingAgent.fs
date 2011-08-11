// ----------------------------------------------------------------------------
// F# async extensions (BatchProcessingAgent.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharp.Control

open System

// ----------------------------------------------------------------------------

/// Agent that can be used to implement batch processing. It creates groups
/// of messages (added using the Enqueue method) and emits them using the 
/// BatchProduced event. A group is produced when it reaches the maximal 
/// size or after the timeout elapses.
type BatchProcessingAgent<'T>(bulkSize, timeout) = 

  let bulkEvent = new Event<'T[]>()
  let agent : Agent<'T> = Agent.Start(fun agent -> 
    let rec loop remainingTime messages = async {
      let start = DateTime.Now
      let! msg = agent.TryReceive(timeout = max 0 remainingTime)
      let elapsed = int (DateTime.Now - start).TotalMilliseconds
      match msg with 
      | Some(msg) when 
            List.length messages = bulkSize - 1 ->
          bulkEvent.Trigger(msg :: messages |> List.rev |> Array.ofList)
          return! loop timeout []
      | Some(msg) ->
          return! loop (remainingTime - elapsed) (msg::messages)
      | None when List.length messages <> 0 -> 
          bulkEvent.Trigger(messages |> List.rev |> Array.ofList)
          return! loop timeout []
      | None -> 
          return! loop timeout [] }
    loop timeout [] )

  /// The event is triggered when a group of messages is collected. The
  /// group is not empty, but may not be of the specified maximal size
  /// (when the timeout elapses before enough messages is collected)
  [<CLIEvent>]
  member x.BatchProduced = bulkEvent.Publish

  /// Sends new message to the agent
  member x.Enqueue v = agent.Post(v)
    