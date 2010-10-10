namespace FSharp.Monad
/// The reader monad.
/// <remarks>
/// This monad comes from Matthew Podwysocki's <see href="http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/07/much-ado-about-monads-reader-edition.aspx"/>.
/// </remarks>
type Reader<'r, 'a> = Reader of ('r -> 'a)

[<AutoOpen>]
module Reader =
  open System
  
  let runReader (Reader r) env = r env
  type ReaderBuilder() =
    member this.Return(a) = Reader (fun _ -> a)

    member this.ReturnFrom(a:Reader<'r,'a>) = a

    member this.Bind(m, k) = Reader (fun r -> runReader (k (runReader m r)) r)

    member this.Zero() = this.Return ()

    member this.Combine(r1, r2) = this.Bind(r1, fun () -> r2)

    member this.TryWith(m, h) =
      Reader (fun env -> try runReader m env
                         with e -> runReader (h e) env)

    member this.TryFinally(m, compensation) =
      Reader (fun env -> try runReader m env
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

    member this.Yield(a) = Reader (fun _ -> a)

    member this.YieldFrom(a:Reader<'r,'a>) = a

  let reader = new ReaderBuilder()

  let ask = Reader(id)
  let asks f = reader {
    let! r = ask
    return (f r) }
  let local f m = Reader (f >> runReader m)