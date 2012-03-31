// ----------------------------------------------------------------------------
// F# async extensions (SlidingWindowAgent.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharp.Control

open System

// ----------------------------------------------------------------------------

/// Agent that implements the "sliding window" functionality. It collects
/// messages added using the Enqueue method and emits them in overlapping 
/// groups of the specified size. For example, given [1,2,3,4,5...] and a 
/// size 3, the produced groups will be [1,2,3], [2,3,4], [3,4,5], ...
type SlidingWindowAgent<'T>(windowSize, ?cancelToken) = 

  // Event used to report groups
  let windowEvent = new Event<_>()

  // Start an agent that remembers partial windows of length 
  // smaller than the count (new agent for every observer)
  let agent = Agent<'T>.Start((fun agent ->
    // The parameter 'lists' contains partial lists and their lengths
    let rec loop lists = async { 
      // Receive the next value
      let! value = agent.Receive()

      // Add new empty list and then the new element to all lists.
      // Then split the lists into 'full' that should be sent
      // to the observer and 'partial' which need more elements.
      let full, partial =
        ((0, []) :: lists)
        |> List.map (fun (length, l) -> length + 1, value::l)
        |> List.partition (fun (length, l) -> length = windowSize)
              
      // Send all full lists to the observer (as arrays)
      for (_, l) in full do
        windowEvent.Trigger(l |> Array.ofSeq |> Array.rev) 
      // Continue looping with incomplete lists
      return! loop partial }

    // Start with an empty list of partial lists
    loop []), ?cancellationToken = cancelToken)

  /// The event is triggered when a group of messages is collected. 
  /// The size of the group is exactly 'count' and the values are
  /// returned in a fresh array.
  [<CLIEvent>]
  member x.WindowProduced = windowEvent.Publish

  /// Sends new message to the agent
  member x.Enqueue v = agent.Post(v)