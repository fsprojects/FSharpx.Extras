[<AutoOpen>]
module FSharpx.Stm.Operators

#if INTERACTIVE
#r @"bin\Debug\FSharpx.Stm.Core.dll"
#endif

type TLog = FSharpx.Stm.Core.TLog
type TVar<'a> = FSharpx.Stm.Core.TVar<'a>

type Stm<'a> = (TLog -> 'a)

let newTVar (value : 'a) : TVar<'a> =
  TLog.NewTVar(value)
  
let readTVar (ref : TVar<'a>) : Stm<'a> =
  fun trans -> trans.ReadTVar(ref)
  
let writeTVar (ref : TVar<'a>) (value : 'a) : Stm<unit> =
  fun trans -> trans.WriteTVar(ref, value)

let retry () : Stm<'a> = 
  fun trans -> trans.Retry<_>()

let orElse (a : Stm<'a>) (b : Stm<'a>) : Stm<'a> = 
  fun trans -> trans.OrElse<_>((fun x -> a x), (fun x -> b x))
  
let atomically (a : Stm<'a>) : 'a =
  TLog.Atomic<_>(fun x -> a x)
  
type StmBuilder () =
    member b.Return(x) : Stm<_> = fun _ -> x

    member b.ReturnFrom(m) : Stm<_> = m

    member b.Bind(p : Stm<_>, rest : _ -> Stm<_>) : Stm<_> = fun trans -> rest (p trans) trans

    member b.Let(p, rest) : Stm<_> = rest p 
    
    member b.Delay(f : unit -> Stm<_>) : Stm<_> = fun trans -> f () trans
 
    member b.Combine(p, q) : Stm<_> = orElse p q

    member b.Zero() = retry ()
    
let stm = new StmBuilder ()


let ifM p x = if p then x else stm.Return(())

let liftM f x = stm { let! x' = x in return f x' }

let sequence (ms : seq<Stm<_> >) : Stm<seq<_> > =
  fun trans -> ms |> Seq.map (fun x -> x trans) |> Seq.cache

let mapM f ms = ms |> Seq.map f |> sequence

let sequence_ (ms : seq<Stm<_> >) : Stm<_> =
  fun trans -> ms |> Seq.iter (fun x -> x trans)

let mapM_ f ms = ms |> Seq.map f |> sequence_
