namespace FSharp.Monad
/// The continuation monad.
/// <remarks>
/// The algorithm is from Wes Dyer <see href="http://blogs.msdn.com/b/wesdyer/archive/2008/01/11/the-marvels-of-monads.aspx"/>.
/// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series <see href="http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx"/>.
/// </remarks>
type Cont<'a, 'r> = Cont of (('a -> 'r) -> 'r)

[<AutoOpen>]
module Continuation =
  open System

  let runCont (Cont c) f = c f
  type ContinuationBuilder() =
    member this.Return(x) = Cont (fun c -> c x)

    member this.ReturnFrom(m: Cont<_,_>) = m

    member this.Bind(m, k) = Cont (fun c -> runCont m (fun x -> runCont (k x) c))

    member this.Zero() = this.Return()

    member this.TryWith(m, h) =
      Cont (fun k -> try runCont m k
                     with e -> runCont (h e) k)

    member this.TryFinally(m, compensation) =
      Cont (fun k -> try runCont m k
                     finally compensation())

    member this.Using(res:#IDisposable, body) =
      this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))

    member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)

    member this.Delay(f) = this.Bind(this.Return(), f)

    member this.While(guard, m) =
      if not(guard()) then this.Zero() else
        this.Bind(m, (fun () -> this.While(guard, m)))

    member this.For(sequence:seq<_>, body) =
      this.Using(sequence.GetEnumerator(),
                 (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

  let cont = ContinuationBuilder()

  let callCC f = Cont(fun k -> runCont (f (fun a -> Cont(fun _ -> k a))) k)