module FSharp.Monad.Reader

open System

type Reader<'r,'a> = 'r -> 'a

/// The reader monad.
/// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/07/much-ado-about-monads-reader-edition.aspx.
type ReaderBuilder() =
  member this.Return(a) : Reader<'r,'a> = fun _ -> a
  member this.ReturnFrom(a:Reader<'r,'a>) = a
  member this.Bind(m:Reader<'r,'a>, k:'a -> Reader<'r,'b>) : Reader<'r,'b> =
    fun r -> (k (m r)) r
  member this.Zero() = this.Return ()
  member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
  member this.TryWith(m:Reader<'r,'a>, h:exn -> Reader<'r,'a>) : Reader<'r,'a> =
    fun env -> try m env
               with e -> (h e) env
  member this.TryFinally(m:Reader<'r,'a>, compensation) : Reader<'r,'a> =
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

let ask : Reader<'r,'r> = id
let asks f = reader {
  let! r = ask
  return (f r) }
let local (f:'r1 -> 'r2) (m:Reader<'r2,'a>) : Reader<'r1, 'a> = f >> m

module Operators =
  open FSharp.Monad.Operators

  let inline returnM x = returnM reader x
  let inline (>>=) m f = bindM reader m f
  let inline (<*>) f m = applyM reader reader f m
  let inline lift f m = liftM reader f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM reader m (fun _ -> f)
