namespace FSharpx

open System
open System.Threading

type internal BatchProcessor<'T> (timeout:int, batchSize:int) = 
    let batchEvent = Event<'T[]>()
    let cts = new CancellationTokenSource()
    let body (agent:MailboxProcessor<'T>) =
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
    let agent : MailboxProcessor<'T> = MailboxProcessor.Start(body, cts.Token)
    /// Triggered when the agent collects a group of messages
    member this.BatchProduced = batchEvent.Publish
    /// Send new message to the agent
    member this.Enqueue(v) = agent.Post(v)
    /// Dispose
    interface IDisposable with
         member x.Dispose() = cts.Cancel()