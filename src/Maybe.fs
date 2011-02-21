namespace FSharp.Monad
/// The maybe monad.
/// <remarks>
/// This monad is my own and uses an 'a option. Others generally make their own Maybe<'a> type from Option<'a>.
/// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series <see href="http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx"/>.
/// </remarks>
module Maybe =
  open System

  type MaybeBuilder() =
    member this.Return(x) = Some(x)

    member this.ReturnFrom(m: 'a option) = m

    member this.Bind(m, f) =
      match m with
      | Some(x) -> f(x)
      | _ -> None

    member this.Zero() = None

    member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)

    member this.Delay(f) = this.Bind(this.Return(), f)

    member this.TryWith(m, h) = this.ReturnFrom(m)

    member this.TryFinally(m, compensation) =
      try this.ReturnFrom(m)
      finally compensation()

    member this.Using(res:#IDisposable, body) =
      this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))

    member this.While(guard, m) =
      if not(guard()) then this.Zero() else
        this.Bind(m, (fun () -> this.While(guard, m)))

    member this.For(sequence:seq<_>, body) =
      this.Using(sequence.GetEnumerator(),
                 (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

    member this.Yield(x) = Some(x)

    member this.YieldFrom(m: 'a option) = m

  let maybe = MaybeBuilder()
