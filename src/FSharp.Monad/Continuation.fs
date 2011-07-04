module FSharp.Monad.Continuation

open System

type Cont<'a,'r> = ('a -> 'r) -> 'r

/// The continuation monad.
/// The algorithm is from Wes Dyer http://blogs.msdn.com/b/wesdyer/archive/2008/01/11/the-marvels-of-monads.aspx.
/// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
type ContinuationBuilder() =
  member this.Return(x) : Cont<'a,'r> = fun c -> c x
  member this.ReturnFrom(m: Cont<'a,'r>) = m
  member this.Bind(m:Cont<'a,'r>, f:'a -> Cont<'b,'r>) : Cont<'b,'r> =
    fun k -> m (fun x -> (f x) k)
  member this.Zero() = this.Return()
  member this.TryWith(m:Cont<'a,'r>, h:exn -> Cont<'a,'r>) : Cont<'a,'r> =
    fun k -> try m k
             with e -> (h e) k
  member this.TryFinally(m:Cont<'a,'r>, compensation) : Cont<'a,'r> =
    fun k -> try m k
             finally compensation()
  member this.Using(res:#IDisposable, body) =
    this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
  member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)
  member this.Delay(f) = this.Bind(this.Return(), f)
  member this.While(guard, m) =
    if not(guard()) then this.Zero() else
      this.Bind(m, fun () -> this.While(guard, m))
  member this.For(sequence:seq<_>, body) =
    this.Using(sequence.GetEnumerator(),
               fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
let cont = ContinuationBuilder()

let callCC f = fun k -> (f (fun a -> (fun _ -> k a))) k

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
