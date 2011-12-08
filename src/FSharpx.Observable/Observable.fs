module FSharpx.Observable

open System
open System.Collections.Generic

let FromEventHandler<'TEventArgs when 'TEventArgs:> EventArgs>
    (addHandler:Action<EventHandler<_>>,
        removeHandler:Action<EventHandler<_>>)  =
    { new IObservable<_> with
        member this.Subscribe(observer:IObserver<_>) =
            let handler = EventHandler<_>(fun _ x -> observer.OnNext x) 
            addHandler.Invoke handler
            let remove () = removeHandler.Invoke handler
            { new IDisposable with member this.Dispose() = remove () }
    }

let FromEvent<'TEventArgs, 'TDelegate when 'TEventArgs:> EventArgs>
    (conversion:Func<Action<'TEventArgs>,'TDelegate>,
        addHandler:Action<'TDelegate>,
            removeHandler:Action<'TDelegate>)  =
    { new IObservable<'TEventArgs> with
        member this.Subscribe(observer:IObserver<_>) =
            let handler = Action<_>(observer.OnNext) |> conversion.Invoke
            addHandler.Invoke handler
            let remove () = removeHandler.Invoke handler
            { new IDisposable with member this.Dispose() = remove () }
    }

let ofSeq<'TItem>(items:'TItem seq) =
    { new IObservable<_> with
        member __.Subscribe(observer:IObserver<_>) =
            for item in items do observer.OnNext item      
            observer.OnCompleted()     
            { new IDisposable with member __.Dispose() = () }
    }

let mapi (f:int -> 'TSource -> 'TResult) (source:IObservable<'TSource>) =
    source 
    |> Observable.scan (fun (i,_) x -> (i+1,Some(x))) (-1,None)
    |> Observable.map (function
        | i,Some(x) -> f i x
        | _,None -> invalidOp "Invalid state"
    )

let takeWhile f (source:IObservable<'TSource>) =
    { new IObservable<_> with
        member __.Subscribe(observer:IObserver<_>) =
            let take = ref true               
            let d = source.Subscribe(fun item ->
                if !take then
                    if f item then observer.OnNext item
                    else take := false; observer.OnCompleted()
            )     
            { new IDisposable with member __.Dispose() = d.Dispose() }
    }

let combineLatest (left:IObservable<'TLeft>) (right:IObservable<'TRight>) =
    let left = left |> Observable.map(fun x -> Some x, None)
    let right = right |> Observable.map(fun x -> None, Some x)
    Observable.merge left right
    |> Observable.scan (fun (_,(l',r')) (l,r) ->
        match l,r,l',r' with
        | Some lv, None, _, Some rv ->
            Some(lv,rv), (l,r')
        | None, Some rv, Some lv, _ ->
            Some(lv,rv), (l',r)
        | _, _, _,_ -> invalidOp "Invalid state"
    ) (None, (None,None))
    |> Observable.choose fst

type internal LinkedList<'T> with
    member xs.pushBack(x:'T) = xs.AddLast(x) |> ignore
    member xs.popFront() = let x = xs.First.Value in xs.RemoveFirst(); x

let zip (left:IObservable<'TLeft>) (right:IObservable<'TRight>) =
    let lefts, rights = LinkedList<'TLeft>(), LinkedList<'TRight>()
    let left = left |> Observable.map Choice1Of2
    let right = right |> Observable.map Choice2Of2
    Observable.merge left right
    |> Observable.choose (fun (c) ->
        match c with
        | Choice1Of2 l when rights.Count = 0 -> lefts.pushBack(l); None
        | Choice1Of2 l -> Some(l, rights.popFront())
        | Choice2Of2 r when lefts.Count = 0 -> rights.pushBack(r); None
        | Choice2Of2 r -> Some(lefts.popFront(), r)
    )

let bufferWithTimeOrCount<'T>  (timeSpan:TimeSpan) (count:int) (source:IObservable<'T>)=
    let timeSpan = int timeSpan.TotalMilliseconds
    let batch = new BatchProcessor<'T>(timeSpan,count) 
    { new IObservable<'T seq> with
        member this.Subscribe(observer:IObserver<'T seq>) =
            let sd = source.Subscribe(fun v -> batch.Enqueue v)
            let dd  = batch.BatchProduced.Subscribe(fun v -> observer.OnNext v)
            { new IDisposable with 
                member this.Dispose() = 
                    sd.Dispose()
                    dd.Dispose()
                    (batch :> IDisposable).Dispose() 
            }
    }

let throttle (milliseconds:int) (source:IObservable<'T>)  =
    let relay (observer:IObserver<'T>) =
        let rec loop () = async {
            let! value = Async.AwaitObservable source
            observer.OnNext value
            do! Async.Sleep milliseconds
            return! loop() }
        loop ()
    { new IObservable<'T> with
        member this.Subscribe(observer:IObserver<'T>) =
            let cts = new System.Threading.CancellationTokenSource()
            Async.StartImmediate(relay observer, cts.Token)
            { new IDisposable with 
                member this.Dispose() = cts.Cancel() 
            }
    }

[<AbstractClass>]  
type internal BasicObserver<'a>() =
    let mutable stopped = false
    abstract Next : value : 'a -> unit
    abstract Error : error : exn -> unit
    abstract Completed : unit -> unit
    interface IObserver<'a> with
        member x.OnNext value = 
            if not stopped then x.Next value
        member x.OnError e = 
            if not stopped then stopped <- true
            x.Error e
        member x.OnCompleted () = 
            if not stopped then stopped <- true
            x.Completed ()

/// Invoke Observer function through specified function
let internal invoke f (w:IObservable<_>) =
    let hook (observer:IObserver<_>) =
        { new BasicObserver<_>() with  
            member x.Next(v) = 
                f (fun () -> observer.OnNext v)
            member x.Error(e) = 
                f (fun () -> observer.OnError(e))
            member x.Completed() = 
                f (fun () -> observer.OnCompleted()) 
        } 
    { new IObservable<_> with 
        member x.Subscribe(observer) =
            w.Subscribe (hook(observer))
    }
 
/// Delay execution of Observer function
let delay milliseconds (observable:IObservable<'a>) =
    let f g =
        async {
            do! Async.Sleep(milliseconds)
            do g ()
        } |> Async.Start
    invoke f observable

/// Execture Observer function on Dispatcher thread
/// <Remarks>For WPF and Silverlight</remarks> 
let onDispatcher (observable:IObservable<'a>) =
#if SILVERLIGHT
    let dispatcher = System.Windows.Deployment.Current.Dispatcher
#else
    let dispatcher = System.Windows.Threading.Dispatcher.CurrentDispatcher
#endif
    let f g =
        dispatcher.BeginInvoke(Action(fun _ -> g())) |> ignore
    invoke f observable