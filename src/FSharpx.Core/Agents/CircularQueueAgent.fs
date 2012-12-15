// -------------------------------------------------------------------------------
// F# async extensions (CircularQueueAgent.fs)
// (c) Tomas Petricek & Ryan Riley, 2011-2012, Available under Apache 2.0 license.
// -------------------------------------------------------------------------------
namespace FSharp.Control

open System
open System.Collections.Generic
open FSharpx.Collections.Mutable

// ----------------------------------------------------------------------------

type internal CircularQueueMessage<'T> = 
  | AsyncEnqueue of 'T[] * int * int * AsyncReplyChannel<unit>
  | Enqueue of 'T[] * int * int
  | AsyncDequeue of int * AsyncReplyChannel<'T[]>

/// Agent that implements an asynchronous circular buffer with blocking
/// enqueue and blocking dequeue operation (this implements the producer-consumer 
/// concurrent programming pattern). The constructor takes the maximal
/// size of the buffer.
type CircularQueueAgent<'T>(maxLength) =
  [<VolatileField>]
  let mutable count = 0
  let agent = Agent.Start(fun agent ->
    
    let queue = new CircularBuffer<'T>(maxLength)

    let rec emptyQueue() = 
      agent.Scan(fun msg ->
        match msg with 
        | AsyncEnqueue(value, offset, count, reply) ->
            Some(enqueueAndContinueWithReply(value, offset, count, reply))
        | Enqueue(value, offset, count) -> Some(enqueueAndContinue(value, offset, count))
        | _ -> None )
    and fullQueue() = 
      agent.Scan(fun msg ->
        match msg with 
        | AsyncDequeue(n, reply) -> Some(dequeueAndContinue(n, reply))
        | _ -> None )
    and runningQueue() = async {
      let! msg = agent.Receive()
      match msg with 
      | AsyncEnqueue(value, offset, count, reply) ->
          return! enqueueAndContinueWithReply(value, offset, count, reply)
      | Enqueue(value, offset, count) -> return! enqueueAndContinue(value, offset, count)
      | AsyncDequeue(n, reply) -> return! dequeueAndContinue(n, reply) }

    and enqueueAndContinueWithReply (value, offset, length, reply) = async {
      reply.Reply()
      queue.Enqueue(value, offset, length)
      count <- queue.Count
      return! chooseState() }
    and enqueueAndContinue (value, offset, length) = async {
      queue.Enqueue(value, offset, length)
      count <- queue.Count
      return! chooseState() }
    and dequeueAndContinue (n, reply) = async {
      reply.Reply(queue.Dequeue(n))
      count <- queue.Count
      return! chooseState() }
    and chooseState() = 
      if queue.Count = 0 then emptyQueue()
      elif queue.Count < maxLength then runningQueue()
      else fullQueue()

    // Start with an empty queue
    emptyQueue() )

  /// Adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.AsyncEnqueue(value: 'T[], offset, count, ?timeout) = 
    agent.PostAndAsyncReply((fun ch -> AsyncEnqueue(value, offset, count, ch)), ?timeout=timeout)

  /// Adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.AsyncEnqueue(segment: ArraySegment<'T>, ?timeout) = 
    agent.PostAndAsyncReply((fun ch -> AsyncEnqueue(segment.Array, segment.Offset, segment.Count, ch)), ?timeout=timeout)

  /// Adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.AsyncEnqueue(value: 'T[], ?timeout) = 
    agent.PostAndAsyncReply((fun ch -> AsyncEnqueue(value, 0, value.Length, ch)), ?timeout=timeout)

  /// Adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.Enqueue(value: 'T[], offset, count) = 
    agent.Post(Enqueue(value, offset, count))

  /// Adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.Enqueue(segment: ArraySegment<'T>) = 
    agent.Post(Enqueue(segment.Array, segment.Offset, segment.Count))

  /// Adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.Enqueue(value: 'T[]) = 
    agent.Post(Enqueue(value, 0, value.Length))

  /// Asynchronously gets item from the queue. If there are no items
  /// in the queue, the operation will block until items are added.
  member x.AsyncDequeue(count, ?timeout) = 
    agent.PostAndAsyncReply((fun ch -> AsyncDequeue(count, ch)), ?timeout=timeout)

  /// Synchronously gets item from the queue. If there are no items
  /// in the queue, the operation will block until items are added.
  /// This method blocks until value is available!
  member x.Dequeue(count, ?timeout) = 
    agent.PostAndReply((fun ch -> AsyncDequeue(count, ch)), ?timeout=timeout)

  /// Gets the number of elements currently waiting in the queue.
  member x.Count = count
