namespace FSharp.Monad
/// The state monad.
/// <remarks>
/// The algorithm is adjusted from my original work off of Brian Beckman's <see href="http://channel9.msdn.com/shows/Going+Deep/Brian-Beckman-The-Zen-of-Expressing-State-The-State-Monad/"/>.
/// The approach was adjusted from Matthew Podwysocki's <see href="http://codebetter.com/blogs/matthew.podwysocki/archive/2009/12/30/much-ado-about-monads-state-edition.aspx"/> and mirrors his final result.
/// </remarks>
module State =
  type State<'a, 's> = State of ('s -> 'a * 's)
  
  let runState (State s) initialState = s initialState
  let getState = State (fun s -> (s,s))
  let putState s = State (fun _ -> ((),s))
  let eval m s = runState m s |> fst
  let exec m s = runState m s |> snd
  type StateBuilder() =
    member this.Return a = State (fun s -> (a,s))

    member this.Bind(m, k) = State (fun s -> let (a, s') = runState m s in runState (k a) s')

  let state = new StateBuilder()