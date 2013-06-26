// ----------------------------------------------------------------------------
// F# async extensions (BlockingQueueAgent.fs)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------
namespace FSharp.Control

open System
open System.Collections.Generic

// ----------------------------------------------------------------------------

type internal BlockingAgentMessage<'T> = 
    | AsyncAdd of 'T * AsyncReplyChannel<unit> 
    | Add of 'T
    | AsyncGet of AsyncReplyChannel<'T>

/// Agent that implements an asynchronous queue with blocking put
/// and blocking get operation (this implements the producer-consumer 
/// concurrent programming pattern). The constructor takes the maximal
/// size of the buffer.
type BlockingQueueAgent<'T>(maxLength) =

    [<VolatileField>]
    let mutable count = 0

    let agent = Agent.Start(fun agent ->
    
        let queue = new Queue<'T>()

        let rec emptyQueue() = 
            agent.Scan(fun msg ->
              match msg with 
              | AsyncAdd(value, reply) -> Some(enqueueAndContinueWithReply(value, reply))
              | Add(value) -> Some(enqueueAndContinue(value))
              | _ -> None )

        and fullQueue() = 
            agent.Scan(fun msg ->
              match msg with 
              | AsyncGet(reply) -> Some(dequeueAndContinue(reply))
              | _ -> None )

        and runningQueue() = async {
            let! msg = agent.Receive()
            match msg with 
            | AsyncAdd(value, reply) -> return! enqueueAndContinueWithReply(value, reply)
            | Add(value) -> return! enqueueAndContinue(value)
            | AsyncGet(reply) -> return! dequeueAndContinue(reply) }

        and enqueueAndContinueWithReply (value, reply) = async {
            reply.Reply() 
            queue.Enqueue(value)
            count <- queue.Count
            return! chooseState() }

        and enqueueAndContinue (value) = async {
            queue.Enqueue(value)
            count <- queue.Count
            return! chooseState() }

        and dequeueAndContinue (reply) = async {
            reply.Reply(queue.Dequeue())
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
    member x.AsyncAdd(v:'T, ?timeout) = 
      agent.PostAndAsyncReply((fun ch -> AsyncAdd(v, ch)), ?timeout=timeout)

    /// Asynchronously adds item to the queue. The operation ends when
    /// there is a place for the item. If the queue is full, the operation
    /// will block until some items are removed. This overload does not
    /// signal the caller that the item has been added.
    member x.Add(v:'T) =
      agent.Post(Add v)

    /// Asynchronously gets item from the queue. If there are no items
    /// in the queue, the operation will block until items are added.
    member x.AsyncGet(?timeout) = 
      agent.PostAndAsyncReply(AsyncGet, ?timeout=timeout)

    /// Synchronously gets item from the queue. If there are no items
    /// in the queue, the operation will block until items are added.
    /// This method blocks until value is available!
    member x.Get(?timeout) = 
      agent.PostAndReply(AsyncGet, ?timeout=timeout)

    /// Gets the number of elements currently waiting in the queue.
    member x.Count = count
