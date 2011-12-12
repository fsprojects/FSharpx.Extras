namespace FSharpx

open System
open System.Collections.Generic

type private CircularBuffer<'T> (bufferSize:int) =
    let buffer = Array.zeroCreate<'T> bufferSize
    let mutable index = 0
    let mutable total = 0
    member this.Add value =
        if bufferSize > 0 then
            buffer.[index] <- value
            index <- (index + 1) % bufferSize
            total <- min (total + 1) bufferSize
    member this.Iter f =     
        let start = if total = bufferSize then index else 0
        for i = 0 to total - 1 do 
            buffer.[(start + i) % bufferSize] |> f

type private message<'T> =
    | Add of IObserver<'T>
    | Remove of IObserver<'T>
    | Next of 'T
    | Completed
    | Error of exn

module private BufferAgent =
    let start (bufferSize:int) =
        let subscribers = LinkedList<_>()
        let buffer = CircularBuffer bufferSize
        MailboxProcessor.Start(fun inbox ->
            let rec loop () = async {
                let! message = inbox.Receive()
                match message with
                | Add observer ->                   
                    subscribers.AddLast observer |> ignore
                    buffer.Iter observer.OnNext
                    return! loop ()
                | Remove observer ->
                    subscribers.Remove observer |> ignore
                    return! loop ()
                | Next value ->
                    for subscriber in subscribers do
                        subscriber.OnNext value
                    buffer.Add value
                    return! loop ()
                | Error e ->
                    for subscriber in subscribers do
                        subscriber.OnError e
                | Completed ->
                    for subscriber in subscribers do
                        subscriber.OnCompleted ()
            }
            loop ()
        )

[<Interface>]
type ISubject<'TIn,'TOut> =
    inherit System.IObserver<'TIn>
    inherit System.IObservable<'TOut>

type ReplaySubject<'T> (bufferSize:int) =
    let bufferSize = max 0 bufferSize
    let agent = BufferAgent.start bufferSize    
    let subscribe observer =
        observer |> Add |> agent.Post
        { new System.IDisposable with
            member this.Dispose () =
                observer |> Remove |> agent.Post
        }

    member this.OnNext value = Next value |> agent.Post
    member this.OnError error = Error error |> agent.Post
    member this.OnCompleted () = Completed |> agent.Post    
    member this.Subscribe(observer:System.IObserver<'T>) = subscribe observer

    interface ISubject<'T,'T> with
        member this.OnNext value = Next value |> agent.Post
        member this.OnError error = Error error |> agent.Post
        member this.OnCompleted () = Completed |> agent.Post
        member this.Subscribe observer = subscribe observer

and Subject<'T>() = inherit ReplaySubject<'T>(0)
