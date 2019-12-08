namespace FSharpx

module Task =
    open System
    open System.Threading
    open System.Threading.Tasks
    open FSharpx.Collections

    /// Task result
    type Result<'T> = 
        /// Task was canceled
        | Canceled
        /// Unhandled exception in task
        | Error of exn 
        /// Task completed successfully
        | Successful of 'T

    let run (t: unit -> Task<'a>) : Result<'a> = 
        try
            let task = t()
            task.Result |> Result<_>.Successful
        with 
        | :? OperationCanceledException -> Result<_>.Canceled
        | :? AggregateException as e ->
            match e.InnerException with
            | :? TaskCanceledException -> Result<_>.Canceled
            | _ -> Result<_>.Error e
        | e -> Result<_>.Error e

    let toAsync (t: Task<'T>): Async<'T> =
        let abegin (cb: AsyncCallback, state: obj) : IAsyncResult = 
            match cb with
            | null -> upcast t
            | cb -> 
                t.ContinueWith(fun (_ : Task<_>) -> cb.Invoke t) |> ignore
                upcast t
        let aend (r: IAsyncResult) = 
            (r :?> Task<'T>).Result
        Async.FromBeginEnd(abegin, aend)

    /// Transforms a Task's first value by using a specified mapping function.
    let inline mapWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) f (m: Task<_>) =
        m.ContinueWith((fun (t: Task<_>) -> f t.Result), token, continuationOptions, scheduler)

    /// Transforms a Task's first value by using a specified mapping function.
    let inline map f (m: Task<_>) =
        m.ContinueWith(fun (t: Task<_>) -> f t.Result)

    let inline bindWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) (f: 'T -> Task<'U>) (m: Task<'T>) =
        m.ContinueWith((fun (x: Task<_>) -> f x.Result), token, continuationOptions, scheduler).Unwrap()

    let inline bind (f: 'T -> Task<'U>) (m: Task<'T>) = 
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

    let foldM f s =
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

    let inline sequence (s:Task<'a> list) =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)

    [<Obsolete("Prefer NuGet package TaskBuilder.fs version FSharp.Control.Tasks.V2.TaskBuilder")>]
    type TaskBuilder(?continuationOptions, ?scheduler, ?cancellationToken) =
        let contOptions = defaultArg continuationOptions TaskContinuationOptions.None
        let scheduler = defaultArg scheduler TaskScheduler.Default
        let cancellationToken = defaultArg cancellationToken CancellationToken.None

        member this.Return x = returnM x

        member this.Bind(m, f) = bindWithOptions cancellationToken contOptions scheduler f m

        member this.Zero() : Task<unit> = this.Return ()

        member this.ReturnFrom (a: Task<'a>) = a

        member this.Run (body : unit -> Task<'a>) = body()

        member this.Delay (body : unit -> Task<'a>) : unit -> Task<'a> = fun () -> this.Bind(this.Return(), body)

        member this.Combine(t1:Task<unit>, t2 : unit -> Task<'b>) : Task<'b> = this.Bind(t1, t2)

        member this.While(guard, body : unit -> Task<unit>) : Task<unit> =
            if not(guard())
            then this.Zero()
            else this.Bind(body(), fun () -> this.While(guard, body))

        member this.TryWith(body : unit -> Task<'a>, catchFn:exn -> Task<'a>) : Task<'a> =
            let continuation (t:Task<'a>) : Task<'a> =
                if t.IsFaulted
                then catchFn(t.Exception.GetBaseException())
                else this.Return(t.Result)

            try body().ContinueWith(continuation).Unwrap()
            with e -> catchFn(e)

        member this.TryFinally(body : unit -> Task<'a>, compensation) : Task<'a> =
            let wrapOk (x:'a) : Task<'a> =
                compensation()
                this.Return x

            let wrapCrash (e:exn) : Task<'a> =
                compensation()
                reraise' e

            this.Bind(this.TryWith(body, wrapCrash), wrapOk)

        member this.Using(res:#IDisposable, body : #IDisposable -> Task<'a>) : Task<'a> =
            let compensation() =
                match res with
                | null -> ()
                | disp -> disp.Dispose()

            this.TryFinally((fun () -> body res), compensation)

        member this.For(sequence:seq<'a>, body : 'a -> Task<unit>) : Task<unit> =
            this.Using( sequence.GetEnumerator()
                      , fun enum -> this.While( enum.MoveNext
                                              , fun () -> body enum.Current
                                              )
                      )

    [<Obsolete("Prefer NuGet package TaskBuilder.fs version FSharp.Control.Tasks.V2.task")>]
    let task = TaskBuilder()

    type TokenToTask<'a> = CancellationToken -> Task<'a>
    type TaskBuilderWithToken(?continuationOptions, ?scheduler) =
        let contOptions = defaultArg continuationOptions TaskContinuationOptions.None
        let scheduler = defaultArg scheduler TaskScheduler.Default

        let lift (t: Task<_>) = fun (_: CancellationToken) -> t

        let bind (t:TokenToTask<'a>) (f : 'a -> TokenToTask<'b>) =
            fun (token: CancellationToken) ->
                (t token).ContinueWith( fun (x: Task<_>) -> f x.Result token
                                      , token
                                      , contOptions
                                      , scheduler
                                      )
                         .Unwrap()
        
        member this.Return x = lift (returnM x)

        member this.Bind(t, f) = bind t f

        member this.Bind(t, f) = bind (lift t) f

        member this.ReturnFrom t = lift t

        member this.ReturnFrom (t:TokenToTask<'a>) = t

        member this.Zero() : TokenToTask<unit> = this.Return ()

        member this.Run (body : unit -> TokenToTask<'a>) = body()

        member this.Delay (body : unit -> TokenToTask<'a>) : unit -> TokenToTask<'a> = fun () -> this.Bind(this.Return(), body)

        member this.Combine(t1 : TokenToTask<unit>, t2 : unit -> TokenToTask<'b>) : TokenToTask<'b> = this.Bind(t1, t2)

        member this.While(guard, body : unit -> TokenToTask<unit>) : TokenToTask<unit> =
            if not(guard())
            then this.Zero()
            else this.Bind(body(), fun () -> this.While(guard, body))

        member this.TryWith(body : unit -> TokenToTask<'a>, catchFn : exn -> TokenToTask<'a>) : TokenToTask<'a> = fun token ->
            let continuation (t:Task<'a>) : Task<'a> =
                if t.IsFaulted
                then catchFn(t.Exception.GetBaseException())
                else this.Return(t.Result)
                <| token

            try (body() token).ContinueWith(continuation).Unwrap()
            with e -> catchFn(e) token

        member this.TryFinally(body : unit -> TokenToTask<'a>, compensation) : TokenToTask<'a> =
            let wrapOk (x:'a) : TokenToTask<'a> =
                compensation()
                this.Return x

            let wrapCrash (e:exn) : TokenToTask<'a> =
                compensation()
                reraise' e

            this.Bind(this.TryWith(body, wrapCrash), wrapOk)

        member this.Using(res:#IDisposable, body : #IDisposable -> TokenToTask<'a>) : TokenToTask<'a> =
            let compensation() =
                match res with
                | null -> ()
                | disp -> disp.Dispose()

            this.TryFinally((fun () -> body res), compensation)

        member this.For(sequence:seq<'a>, body : 'a -> TokenToTask<unit>) : TokenToTask<unit> =
            this.Using( sequence.GetEnumerator()
                      , fun enum -> this.While( enum.MoveNext
                                              , fun () -> body enum.Current
                                              )
                      )

    /// Creates a single Task<unit> that will complete when all of the Task<unit> objects in an enumerable collection have completed.
    let inline WhenAllUnits (units:seq<Task<unit>>) : Task<unit> =
        task {
            let! (_:unit[]) = Task.WhenAll units
            return ()
        }

    /// Converts a Task into Task<unit>
    let inline ToTaskUnit (t:Task) =
        let inline continuation _ = ()
        t.ContinueWith continuation

    /// Creates a task that runs the given task and ignores its result.
    let inline Ignore t = bind (fun _ -> returnM ()) t

    /// Active pattern that matches on flattened inner exceptions in an AggregateException
    let (|AggregateExn|_|) (e:exn) =
        match e with
        | :? AggregateException as ae ->
            ae.Flatten().InnerExceptions
            |> List.ofSeq
            |> Some
        | _ -> None

    /// Creates a task that executes a specified task.
    /// If this task completes successfully, then this function returns Choice1Of2 with the returned value.
    /// If this task raises an exception before it completes then return Choice2Of2 with the raised exception.
    let Catch (t:Task<'a>) : Task<Choice<'a, exn>> =
        task {
            try let! r = t
                return Choice1Of2 r
            with e ->
                let e' = match e with
                         | AggregateExn [inner] -> inner
                         | x                    -> x
                return Choice2Of2 e'
        }

    /// Creates a task that executes all the given tasks.
    let Parallel (tasks : seq<unit -> Task<'a>>) : (Task<'a[]>) =
        tasks
        |> Seq.map (fun t -> t())
        |> Task.WhenAll

    /// Creates a task that executes all the given tasks.
    /// This function doesn't throw exceptions, but instead returns an array of Choices.
    let ParallelCatch (tasks : seq<unit -> Task<'a>>) : (Task<Choice<'a, exn>[]>) =
        let catch t () =
            Catch <| t()
        tasks
        |> Seq.map catch
        |> Parallel

    /// common code for ParallelCatchWithThrottle and ParallelWithThrottle
    let private ParallelWithThrottleCustom transformResult throttle (tasks : seq<unit -> Task<'a>>) : (Task<'b[]>) =
        task {
            use semaphore = new SemaphoreSlim(throttle)
            let throttleTask (t:unit->Task<'a>) () : Task<'b> =
                task {
                    do! semaphore.WaitAsync() |> ToTaskUnit
                    let! result = Catch <| t()
                    semaphore.Release() |> ignore
                    return transformResult result
                }
            return! tasks
                    |> Seq.map throttleTask
                    |> Parallel
        }

    /// Creates a task that executes all the given tasks.
    /// This function doesn't throw exceptions, but instead returns an array of Choices.
    /// The paralelism is throttled, so that at most `throttle` tasks run at one time.
    let ParallelCatchWithThrottle throttle (tasks : seq<unit -> Task<'a>>) : (Task<Choice<'a, exn>[]>) =
        ParallelWithThrottleCustom id throttle tasks

    /// Creates a task that executes all the given tasks.
    /// The paralelism is throttled, so that at most `throttle` tasks run at one time.
    let ParallelWithThrottle throttle (tasks : seq<unit -> Task<'a>>) : (Task<'a[]>) =
        ParallelWithThrottleCustom Choice.getOrReraise throttle tasks