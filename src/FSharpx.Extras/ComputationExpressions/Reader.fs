namespace FSharpx

module Reader =
    open System
    open FSharpx.Collections

    type Reader<'R,'T> = 'R -> 'T

    let bind k m = fun r -> (k (m r)) r
    
    /// The reader monad.
    /// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/07/much-ado-about-monads-reader-edition.aspx.
    type ReaderBuilder() =
        member this.Return(a) : Reader<'R,'T> = fun _ -> a
        member this.ReturnFrom(a:Reader<'R,'T>) = a
        member this.Bind(m:Reader<'R,'T>, k:'T -> Reader<'R,'U>) : Reader<'R,'U> = bind k m
        member this.Zero() = this.Return ()
        member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
        member this.TryWith(m:Reader<'R,'T>, h:exn -> Reader<'R,'T>) : Reader<'R,'T> =
            fun env -> try m env
                       with e -> (h e) env
        member this.TryFinally(m:Reader<'R,'T>, compensation) : Reader<'R,'T> =
            fun env -> try m env
                       finally compensation()
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            if not(guard()) then this.Zero() else
                this.Bind(m, (fun () -> this.While(guard, m)))
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
    let reader = new ReaderBuilder()
    
    let ask : Reader<'R,'R> = id

    let asks f = reader {
        let! r = ask
        return (f r) }

    let local (f:'r1 -> 'r2) (m:Reader<'r2,'T>) : Reader<'r1, 'T> = f >> m
    
    open Operators
    
    /// Inject a value into the Reader type
    let inline returnM x = returnM reader x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM reader m f
    /// Flipped >>=
    let inline (=<<) f m = bindM reader m f
    /// Sequential application
    let inline (<*>) f m = applyM reader reader f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Transforms a Reader value by using a specified mapping function.
    let inline map f m = liftM reader f m
    /// Infix map
    let inline (<!>) f m = map f m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    /// Sequentially compose two reader actions, discarding any value produced by the first
    let inline (>>.) m f = bindM reader m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)