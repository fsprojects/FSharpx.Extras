namespace FSharpx

open System
open System.Collections.Generic
open FSharpx.Collections

/// The continuation monad.
/// The algorithm is from Wes Dyer http://blogs.msdn.com/b/wesdyer/archive/2008/01/11/the-marvels-of-monads.aspx.
/// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
/// Current implementation from Matt's gist at https://gist.github.com/628956
type Cont<'T,'r> = ('T -> 'r) -> (exn -> 'r) -> 'r

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Continuation =
    
    let private protect f x cont econt =
        let res = try Choice1Of2 (f x) with err -> Choice2Of2 err
        match res with
        | Choice1Of2 v -> cont v
        | Choice2Of2 v -> econt v
    
    let runCont (c:Cont<_,_>) cont econt = c cont econt
    let throw exn : Cont<'T,'r> = fun cont econt -> econt exn
    let callcc (f: ('T -> Cont<'b,'r>) -> Cont<'T,'r>) : Cont<'T,'r> =
        fun cont econt -> runCont (f (fun a -> (fun _ _ -> cont a))) cont econt
    let bind f comp1 = 
        fun cont econt ->
            runCont comp1 (fun a -> protect f a (fun comp2 -> runCont comp2 cont econt) econt) econt     

    type ContinuationBuilder() =
        member this.Return(a) : Cont<_,_> = fun cont econt -> cont a
        member this.ReturnFrom(comp:Cont<_,_>) = comp
        member this.Bind(comp1, f) = bind f comp1
        member this.Catch(comp:Cont<_,_>) : Cont<Choice<_, exn>, _> = fun cont econt ->
            runCont comp (fun v -> cont (Choice1Of2 v)) (fun err -> cont (Choice2Of2 err))
        member this.Zero() =
            this.Return ()
        member this.TryWith(tryBlock, catchBlock) =
            this.Bind(this.Catch tryBlock, (function Choice1Of2 v -> this.Return v 
                                                   | Choice2Of2 exn -> catchBlock exn))
        member this.TryFinally(tryBlock, finallyBlock) =
            this.Bind(this.Catch tryBlock, (function Choice1Of2 v -> finallyBlock(); this.Return v 
                                                   | Choice2Of2 exn -> finallyBlock(); throw exn))
        member this.Using(res:#IDisposable, body) =
            this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
        member this.Combine(comp1, comp2) = this.Bind(comp1, (fun () -> comp2))
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(pred, body) =
            if pred() then this.Bind(body, (fun () -> this.While(pred,body))) else this.Return ()
        member this.For(items:seq<_>, body) =
            this.Using(items.GetEnumerator(),
                (fun enum -> this.While((fun () -> enum.MoveNext()), this.Delay(fun () -> body enum.Current))))
    let cont = ContinuationBuilder()
    
    open Operators
    
    /// Inject a value into the Cont type
    let inline returnM x = returnM cont x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM cont m f
    /// Flipped >>=
    let inline (=<<) f m = bindM cont m f
    /// Sequential application
    let inline (<*>) f m = applyM cont cont f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Transforms a Cont value by using a specified mapping function.
    let inline map f m = liftM cont f m
    /// Infix map
    let inline (<!>) f m = map f m
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    /// Sequentially compose two continuation actions, discarding any value produced by the first
    let inline (>>.) m f = bindM cont m (fun _ -> f)
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

/// The coroutine type from http://fssnip.net/7M
type Coroutine() =
    let tasks = new System.Collections.Generic.Queue<Cont<unit,unit>>()

    member this.Put(task) =
        let withYield = Continuation.cont {
            do! Continuation.callcc <| fun exit ->
                task <| fun () ->
                Continuation.callcc <| fun c ->
                tasks.Enqueue(c())
                exit()
            if tasks.Count <> 0 then
                do! tasks.Dequeue() }
        tasks.Enqueue(withYield)
        
    member this.Run() =
        Continuation.runCont (tasks.Dequeue()) ignore raise
