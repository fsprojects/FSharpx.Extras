module FSharp.Monad.State

open System

type State<'a, 's> = 's -> 'a * 's

let getState = fun s -> (s,s)
let putState s = fun _ -> ((),s)
let eval m s = m s |> fst
let exec m s = m s |> snd
let empty = fun s -> ((), s)

/// The state monad.
/// The algorithm is adjusted from my original work off of Brian Beckman's http://channel9.msdn.com/shows/Going+Deep/Brian-Beckman-The-Zen-of-Expressing-State-The-State-Monad/.
/// The approach was adjusted from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2009/12/30/much-ado-about-monads-state-edition.aspx and mirrors his final result.
type StateBuilder() =
  member this.Return(a) : State<'a,'s> = fun s -> (a,s)
  member this.ReturnFrom(m:State<'a,'s>) = m
  member this.Bind(m:State<'a,'s>, k:'a -> State<'b,'s>) : State<'b,'s> =
    fun s -> let (a, s') = m s in (k a) s'
  member this.Zero() = this.Return ()
  member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
  member this.TryWith(m:State<'a,'s>, h:exn -> State<'a,'s>) : State<'a,'s> =
    fun env -> try m env
               with e -> (h e) env
  member this.TryFinally(m:State<'a,'s>, compensation) : State<'a,'s> =
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
let state = new StateBuilder()

module Operators =
  open FSharp.Monad.Operators

  let inline returnM x = returnM state x
  let inline (>>=) m f = bindM state m f
  let inline (<*>) f m = applyM state state f m
  let inline lift f m = liftM state f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM state m (fun _ -> f)
