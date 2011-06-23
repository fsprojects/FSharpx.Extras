namespace FSharp.Monad
/// The maybe monad.
/// This monad is my own and uses an 'a option. Others generally make their own Maybe<'a> type from Option<'a>.
/// The builder approach is from Matthew Podwysocki's excellent Creating Extended Builders series http://codebetter.com/blogs/matthew.podwysocki/archive/2010/01/18/much-ado-about-monads-creating-extended-builders.aspx.
module Maybe =
  open System

  type MaybeBuilder() =
    member this.Return(x) = Some x
    member this.ReturnFrom(m: 'a option) = m
    member this.Bind(m, f) = Option.bind f m
    member this.Zero() = None
    member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)
    member this.Delay(f) = this.Bind(this.Return(), f)
    member this.TryWith(m, h) = this.ReturnFrom(m)
    member this.TryFinally(m, compensation) =
      try this.ReturnFrom(m)
      finally compensation()
    member this.Using(res:#IDisposable, body) =
      this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())
    member this.While(guard, m) =
      if not(guard()) then this.Zero() else
        this.Bind(m, fun () -> this.While(guard, m))
    member this.For(sequence:seq<_>, body) =
      this.Using(sequence.GetEnumerator(),
                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
  let maybe = MaybeBuilder()

  module Operators =
    let inline mreturn x = maybe.Return x
    let inline (>>=) m f = maybe.Bind(m, f)
    let inline (<*>) f m = f >>= fun f' -> m >>= fun m' -> mreturn (f' m')
    let inline lift f m = mreturn f <*> m
    let inline (<!>) f m = lift f m
    let inline lift2 f a b = mreturn f <*> a <*> b
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    let inline (>>.) m f = maybe.Bind(m, fun _ -> f)