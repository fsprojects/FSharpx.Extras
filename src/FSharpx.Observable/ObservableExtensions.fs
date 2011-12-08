namespace FSharpx

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open FSharpx.Observable

[<Extension>]
type ObservableExtensions private () =
    [<Extension>]
    static member
        ToObservable<'TItem>
            (source:IEnumerable<'TItem>) =
        source |> Observable.ofSeq
    [<Extension>]
    static member 
        Subscribe<'TSource>
            (source:IObservable<'TSource>, 
             action:Action<'TSource>) =
        source |> Observable.subscribe action.Invoke
    [<Extension>]
    static member 
        Where<'TSource>
            (source:IObservable<'TSource>,
             predicate:Func<'TSource,bool>) =
        source |> Observable.filter predicate.Invoke
    [<Extension>]
    static member 
        Select<'TSource,'TResult>
            (source:IObservable<'TSource>,
             selector:Func<'TSource,'TResult>) =
        source |> Observable.map selector.Invoke
    [<Extension>]
    static member 
        Select<'TSource,'TResult>
            (source:IObservable<'TSource>,
             selector:Func<'TSource,int,'TResult>) =
        source |> Observable.mapi (fun i x -> selector.Invoke(x,i))
    [<Extension>]
    static member 
        SelectMany<'TSource,'TCollection,'TResult>
            (source:IObservable<'TSource>,
             collectionSelector:Func<'TSource,IEnumerable<'TCollection>>,
             resultSelector:Func<'TSource,'TCollection,'TResult>) =
        { new IObservable<'TResult> with
            member this.Subscribe(observer:IObserver<_>) =
                let disposable = source.Subscribe(fun s -> 
                    let cs = collectionSelector.Invoke s
                    for c in cs do
                        let r = resultSelector.Invoke(s,c)
                        observer.OnNext(r)
                )
                { new IDisposable with 
                    member this.Dispose() = disposable.Dispose() }
        }
    [<Extension>]
    static member 
        TakeWhile<'TSource>
            (source:IObservable<'TSource>,
             f:Func<'TSource, bool>) =
        source |> Observable.takeWhile f.Invoke
    [<Extension>]
    static member 
        Merge<'TSource>
            (source:IObservable<'TSource>, 
             sources:IEnumerable<IObservable<'TSource>>) =
        let rec merge source = function
            | [] -> source
            | source'::sources' ->
                let result = Observable.merge source source'
                merge result sources'
        sources |> Seq.toList |> merge source
    [<Extension>]
    static member 
        Merge<'TSource>
            (source:IObservable<'TSource>, 
             [<ParamArray>] sources:IObservable<'TSource> []) =
        ObservableExtensions.Merge(source,sources |> Seq.ofArray)
    [<Extension>]
    static member 
        Scan<'TSource,'TAccumulate>
            (source:IObservable<'TSource>,
             seed:'TAccumulate,
             f:Func<'TAccumulate,'TSource,'TAccumulate>) =
        source |> Observable.scan (fun acc x -> f.Invoke(acc,x)) seed
    [<Extension>]
    static member 
        CombineLatest<'TLeft,'TRight,'TResult>
            (left:IObservable<'TLeft>,
             right:IObservable<'TRight>,
             selector:Func<'TLeft, 'TRight, 'TResult>) =
        Observable.combineLatest left right
        |> Observable.map selector.Invoke
    [<Extension>]
    static member 
        Zip<'TLeft,'TRight,'TResult>
            (left:IObservable<'TLeft>,
             right:IObservable<'TRight>,
             selector:Func<'TLeft, 'TRight, 'TResult>) =
        Observable.zip left right
        |> Observable.map selector.Invoke
    [<Extension>]
    static member 
        Delay<'TSource>
            (source:IObservable<'TSource>,
             milliseconds:int) =
        source |> Observable.delay milliseconds
    [<Extension>]
    static member 
        BufferWithTimeOrCount<'TSource>
            (source:IObservable<'TSource>, 
             timeSpan:TimeSpan,
             count:int) =
        source |> Observable.bufferWithTimeOrCount timeSpan count
    [<Extension>]
    static member 
        Throttle<'TSource>
            (source:IObservable<'TSource>, 
             dueTime:TimeSpan) =
        let dueTime = int dueTime.TotalMilliseconds
        source |> Observable.throttle dueTime
    [<Extension>]
    static member 
        OnDispatcher<'TSource>
            (source:IObservable<'TSource>) =
        source |> Observable.onDispatcher
