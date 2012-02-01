// -------------------------------------------------------------------------------
// F# async extensions (CircularBufferAgent.fs)
// (c) Tomas Petricek & Ryan Riley, 2011-2012, Available under Apache 2.0 license.
// -------------------------------------------------------------------------------
namespace FSharp.Control

open System
open System.Collections.Generic
open FSharpx

// ----------------------------------------------------------------------------

type internal CircularBufferMessage<'T> = 
  | Enqueue of 'T[] * int * int * AsyncReplyChannel<unit> 
  | Dequeue of int * AsyncReplyChannel<'T[]>

/// Agent that implements an asynchronous circular buffer with blocking
/// enqueue and blocking dequeue operation (this implements the producer-consumer 
/// concurrent programming pattern). The constructor takes the maximal
/// size of the buffer.
type CircularBufferAgent<'T>(maxLength) =
  [<VolatileField>]
  let mutable count = 0
  let agent = Agent.Start(fun agent ->
    
    let queue = new CircularBuffer<_>(maxLength)

    let rec emptyQueue() = 
      agent.Scan(fun msg ->
        match msg with 
        | Enqueue(value, offset, count, reply) -> Some(enqueueAndContinue(value, offset, count, reply))
        | _ -> None )
    and fullQueue() = 
      agent.Scan(fun msg ->
        match msg with 
        | Dequeue(n, reply) -> Some(dequeueAndContinue(n, reply))
        | _ -> None )
    and runningQueue() = async {
      let! msg = agent.Receive()
      match msg with 
      | Enqueue(value, offset, count, reply) -> return! enqueueAndContinue(value, offset, count, reply)
      | Dequeue(n, reply) -> return! dequeueAndContinue(n, reply) }

    and enqueueAndContinue (value, offset, length, reply) = async {
      reply.Reply() 
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

  /// Asynchronously adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.AsyncEnqueue(value: 'T[], offset, count, ?timeout) = 
    agent.PostAndAsyncReply((fun ch -> Enqueue(value, offset, count, ch)), ?timeout=timeout)

  /// Asynchronously adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.AsyncEnqueue(segment: ArraySegment<'T>, ?timeout) = 
    agent.PostAndAsyncReply((fun ch -> Enqueue(segment.Array, segment.Offset, segment.Count, ch)), ?timeout=timeout)

  /// Asynchronously adds item to the queue. The operation ends when
  /// there is a place for the item. If the queue is full, the operation
  /// will block until some items are removed.
  member x.AsyncEnqueue(value: 'T[], ?timeout) = 
    agent.PostAndAsyncReply((fun ch -> Enqueue(value, 0, value.Length, ch)), ?timeout=timeout)

  /// Asynchronously gets item from the queue. If there are no items
  /// in the queue, the operation will block until items are added.
  member x.AsyncDequeue(count, ?timeout) = 
    agent.PostAndAsyncReply((fun ch -> Dequeue(count, ch)), ?timeout=timeout)

  /// Synchronously gets item from the queue. If there are no items
  /// in the queue, the operation will block until items are added.
  /// This method blocks until value is available!
  member x.Dequeue(count, ?timeout) = 
    agent.PostAndReply((fun ch -> Dequeue(count, ch)), ?timeout=timeout)

  /// Gets the number of elements currently waiting in the queue.
  member x.Count = count
