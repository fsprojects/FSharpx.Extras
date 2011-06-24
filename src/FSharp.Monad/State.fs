namespace FSharp.Monad
/// The state monad.
/// The algorithm is adjusted from my original work off of Brian Beckman's http://channel9.msdn.com/shows/Going+Deep/Brian-Beckman-The-Zen-of-Expressing-State-The-State-Monad/.
/// The approach was adjusted from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2009/12/30/much-ado-about-monads-state-edition.aspx and mirrors his final result.
module State =
  open System

  type State<'a, 's> = State of ('s -> 'a * 's)

  let runState (State s) initialState = s initialState
  let getState = State (fun s -> (s,s))
  let putState s = State (fun _ -> ((),s))
  let eval m s = runState m s |> fst
  let exec m s = runState m s |> snd
  let empty = State(fun s -> ((), s))

  type StateBuilder() =
    member this.Return a = State (fun s -> (a,s))
    member this.ReturnFrom(m:State<'a,'s>) = m
    member this.Bind(m, k) = State (fun s -> let (a, s') = runState m s in runState (k a) s')
    member this.Zero() = this.Return ()
    member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)
    member this.TryWith(m, h) =
      State (fun env -> try runState m env
                         with e -> runState (h e) env)
    member this.TryFinally(m, compensation) =
      State (fun env -> try runState m env
                         finally compensation())
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
    let inline mreturn x = state.Return x
    let inline (>>=) m f = state.Bind(m, f)
    let inline (<*>) f m = f >>= fun f' -> m >>= fun m' -> mreturn (f' m')
    let inline lift f m = mreturn f <*> m
    let inline (<!>) f m = lift f m
    let inline lift2 f a b = mreturn f <*> a <*> b
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    let inline (>>.) m f = state.Bind(m, fun _ -> f)
