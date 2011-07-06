module FSharp.Monad.Continuation

open System

/// The continuation monad.
/// The algorithm is from Wes Dyer http://blogs.msdn.com/b/wesdyer/archive/2008/01/11/the-marvels-of-monads.aspx.
/// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
/// Current implementation from Matt's gist at https://gist.github.com/628956
type Cont<'a,'r> =
  abstract Run : ('a -> 'r) * (exn -> 'r) -> 'r

let private protect f x cont econt =
  let res = try Choice1Of2 (f x) with err -> Choice2Of2 err
  match res with
  | Choice1Of2 v -> cont v
  | Choice2Of2 v -> econt v

let runCont (c:Cont<_,_>) cont econt = c.Run(cont, econt)
let throw exn = { new Cont<_,_> with member x.Run (cont,econt) = econt exn }
let callCC f =
  { new Cont<_,_> with
      member x.Run(cont, econt) =
        runCont (f (fun a -> { new Cont<_,_> with member x.Run(_,_) = cont a })) cont econt }
 
type ContinuationBuilder() =
  member this.Return(a) = 
    { new Cont<_,_> with member x.Run(cont, econt) = cont a }
  member this.ReturnFrom(comp:Cont<_,_>) = comp
  member this.Bind(comp1, f) = 
    { new Cont<_,_> with 
        member x.Run (cont, econt) = 
          runCont comp1 (fun a -> protect f a (fun comp2 -> runCont comp2 cont econt) econt) econt }
  member this.Catch(comp:Cont<_,_>) =
    { new Cont<Choice<_, exn>,_> with 
        member x.Run (cont, econt) = 
          runCont comp (fun v -> cont (Choice1Of2 v)) (fun err -> cont (Choice2Of2 err)) }
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
    this.Using(items.GetEnumerator(), (fun enum -> this.While((fun () -> enum.MoveNext()), this.Delay(fun () -> body enum.Current))))
let cont = ContinuationBuilder()

module Operators =
  open FSharp.Monad.Operators

  let inline returnM x = returnM cont x
  let inline (>>=) m f = bindM cont m f
  let inline (<*>) f m = applyM cont cont f m
  let inline lift f m = liftM cont f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM cont m (fun _ -> f)
