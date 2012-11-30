#if NET40
namespace FSharpx
#nowarn "40"

open System
open System.Collections
open System.Collections.Generic

module Task =
    open System.Threading
    open System.Threading.Tasks

    /// Task result
    type 'a Result = 
    /// Task was canceled
    | Canceled
    /// Unhandled exception in task
    | Error of exn 
    /// Task completed successfully
    | Successful of 'a

    let run (t: unit -> Task<_>) = 
        try
            let task = t()
            task.Result |> Result.Successful
        with 
        | :? OperationCanceledException -> Result.Canceled
        | :? AggregateException as e ->
            match e.InnerException with
            | :? TaskCanceledException -> Result.Canceled
            | _ -> Result.Error e
        | e -> Result.Error e

    let toAsync (t: Task<'a>): Async<'a> =
        let abegin (cb: AsyncCallback, state: obj) : IAsyncResult = 
            match cb with
            | null -> upcast t
            | cb -> 
                t.ContinueWith(fun (_ : Task<_>) -> cb.Invoke t) |> ignore
                upcast t
        let aend (r: IAsyncResult) = 
            (r :?> Task<'a>).Result
        Async.FromBeginEnd(abegin, aend)

    /// Transforms a Task's first value by using a specified mapping function.
    let inline mapWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) f (m: Task<_>) =
        m.ContinueWith((fun (t: Task<_>) -> f t.Result), token, continuationOptions, scheduler)

    /// Transforms a Task's first value by using a specified mapping function.
    let inline map f (m: Task<_>) =
        m.ContinueWith(fun (t: Task<_>) -> f t.Result)

    let inline bindWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) (f: 'a -> Task<'b>) (m: Task<'a>) =
        m.ContinueWith((fun (x: Task<_>) -> f x.Result), token, continuationOptions, scheduler).Unwrap()

    let inline bind (f: 'a -> Task<'b>) (m: Task<'a>) = 
        m.ContinueWith(fun (x: Task<_>) -> f x.Result).Unwrap()

    let inline returnM a = 
        let s = TaskCompletionSource()
        s.SetResult a
        s.Task

    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bind f m
    /// Flipped >>=
    let inline (=<<) f m = bind f m
    /// Sequentially compose two either actions, discarding any value produced by the first
    let inline (>>.) m1 m2 = m1 >>= (fun _ -> m2)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = 
        a >>= fun aa -> b >>= fun bb -> f aa bb |> returnM

    /// Sequential application
    let inline ap x f = lift2 id f x

    /// Sequential application
    let inline (<*>) f x = ap x f

    /// Infix map
    let inline (<!>) f x = map f x

    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) a b = lift2 (fun _ z -> z) a b
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) a b = lift2 (fun z _ -> z) a b
    
    type TaskBuilder(?continuationOptions, ?scheduler, ?cancellationToken) =
        let contOptions = defaultArg continuationOptions TaskContinuationOptions.None
        let scheduler = defaultArg scheduler TaskScheduler.Default
        let cancellationToken = defaultArg cancellationToken CancellationToken.None
        member this.Return x = returnM x
        member this.Zero() = returnM ()
        member this.ReturnFrom (a: 'a Task) = a
        member this.Bind(m, f) = bindWithOptions cancellationToken contOptions scheduler f m
        member this.Combine(comp1, comp2) =
            this.Bind(comp1, comp2)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m(), fun () -> this.While(guard, m))
        member this.TryFinally(m, compensation) =
            try this.ReturnFrom m
            finally compensation()
        member this.Using(res: #IDisposable, body: #IDisposable -> _ Task) =
            this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
        member this.For(sequence: seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                                 fun enum -> this.While(enum.MoveNext, fun () -> body enum.Current))
        member this.Delay (f: unit -> 'a Task) = f
        member this.Run (f: unit -> 'a Task) = f()

#endif
