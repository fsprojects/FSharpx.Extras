namespace FSharpx

module Writer =
    open System
    open FSharpx.Collections
        
    type Writer<'W, 'T> = unit -> 'T * 'W

    let bind (m: Monoid<_>) (k:'T -> Writer<'W,'U>) (writer:Writer<'W,'T>) : Writer<'W,'U> =
        fun () ->
            let (a, w) = writer()
            let (a', w') = (k a)()
            (a', m.Combine(w, w'))

    /// Inject a value into the Writer type
    let returnM (monoid: Monoid<_>) a = 
        fun () -> (a, monoid.Zero())
    
    /// The writer monad.
    /// This monad comes from Matthew Podwysocki's http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx.
    type WriterBuilder<'W>(monoid: 'W Monoid) =
        member this.Return(a) : Writer<'W,'T> = returnM monoid a
        member this.ReturnFrom(w:Writer<'W,'T>) = w
        member this.Bind(writer, k) = bind monoid k writer
        member this.Zero() = this.Return ()
        member this.TryWith(writer:Writer<'W,'T>, handler:exn -> Writer<'W,'T>) : Writer<'W,'T> =
            fun () -> try writer()
                      with e -> (handler e)()
        member this.TryFinally(writer, compensation) =
            fun () -> try writer()
                      finally compensation()
        member this.Using<'d,'W,'T when 'd :> IDisposable and 'd : null>(resource : 'd, body : 'd -> Writer<'W,'T>) : Writer<'W,'T> =
            this.TryFinally(body resource, fun () -> match resource with null -> () | disp -> disp.Dispose())
        member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)
        member this.Delay(f) = this.Bind(this.Return (), f)
        member this.While(guard, m) =
            match guard() with
            | true -> this.Bind(m, (fun () -> this.While(guard, m))) 
            | _        -> this.Zero()
        member this.For(sequence:seq<'T>, body:'T -> Writer<'W,unit>) =
            this.Using(sequence.GetEnumerator(), 
                fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

    let writer = WriterBuilder(List.monoid<string>)

    let tell   w = fun () -> ((), w)
    let listen m = fun () -> let (a, w) = m() in ((a, w), w)
    let pass   m = fun () -> let ((a, f), w) = m() in (a, f w)
    
    let listens monoid f m = 
        let writer = WriterBuilder(monoid)
        writer {
            let! (a, b) = m
            return (a, f b) }
    
    let censor monoid (f:'w1 -> 'w2) (m:Writer<'w1,'T>) : Writer<'w2,'T> =
        let writer = WriterBuilder(monoid)
        writer { let! a = m
                 return (a, f)
               } |> pass

    open Operators
    
    let inline private ret x = returnM writer x

    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM writer m f

    /// Flipped >>=
    let inline (=<<) f m = bindM writer m f

    /// Sequential application
    let inline (<*>) f m = applyM writer writer f m

    /// Sequential application
    let inline ap m f = f <*> m

    /// Transforms a Writer value by using a specified mapping function.
    let inline map f m = liftM writer f m

    /// Infix map
    let inline (<!>) f m = map f m

    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = ret f <*> a <*> b

    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y

    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y

    /// Sequentially compose two state actions, discarding any value produced by the first
    let inline (>>.) m f = bindM writer m (fun _ -> f)

    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g

    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (ret s)

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (ret [])

    let inline mapM f x = sequence (List.map f x)