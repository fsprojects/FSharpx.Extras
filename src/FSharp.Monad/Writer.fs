namespace FSharp.Monad
/// The writer monad.
/// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx.
module Writer =
  open System
  open Monoid
  
  type Writer<'w, 'a> = Writer of (unit -> 'a * 'w)

  let runWriter (Writer w) : ('a * 'w) = w()
  type WriterBuilder() =
    member this.Return(a) = Writer (fun () -> a, mempty())
    member this.ReturnFrom(w:Writer<'w,'a>) = w
    member this.Bind(writer, k) =
      Writer (fun () ->
        let (a, w) = runWriter writer
        let (a', w') = runWriter (k a)
        in  (a', mappend w w'))
    member this.Zero() = this.Return ()
    member this.TryWith(writer, handler) =
      Writer(fun () -> try runWriter writer
                       with e -> runWriter (handler e))
    member this.TryFinally(writer, compensation) =
      Writer(fun () -> try runWriter writer
                       finally compensation())
    member this.Using<'d,'w,'a when 'd :> IDisposable and 'd : null>(resource : 'd, body : 'd -> Writer<'w,'a>) =
      this.TryFinally(body resource, fun () -> match resource with null -> () | disp -> disp.Dispose())
    member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)
    member this.Delay(f) = this.Bind(this.Return (), f)
    member this.While(guard, m) =
      match guard() with
      | true -> this.Bind(m, (fun () -> this.While(guard, m))) 
      | _    -> this.Zero()
    member this.For(sequence:seq<'a>, body:'a -> Writer<'w,unit>) =
      this.Using(sequence.GetEnumerator(), 
                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
  let writer = new WriterBuilder()

  let tell   w = Writer (fun () -> (), w)
  let listen m = Writer (fun () -> let (a, w) = runWriter m in ((a, w), w))
  let pass   m = Writer (fun () -> let ((a, f), w) = runWriter m in (a, f w))

  let listens f m = writer {
    let! (a, w) = m
    return (a, f w) }

  let censor f m =
    writer { let! a = m
             return (a, f)
           } |> pass

  module Operators =
    let inline mreturn x = writer.Return x
    let inline (>>=) m f = writer.Bind(m, f)
    let inline (<*>) f m = f >>= fun f' -> m >>= fun m' -> mreturn (f' m')
    let inline lift f m = mreturn f <*> m
    let inline (<!>) f m = lift f m
    let inline lift2 f a b = mreturn f <*> a <*> b
    let inline ( *>) x y = lift2 (fun _ z -> z) x y
    let inline ( <*) x y = lift2 (fun z _ -> z) x y
    let inline (>>.) m f = writer.Bind(m, fun _ -> f)