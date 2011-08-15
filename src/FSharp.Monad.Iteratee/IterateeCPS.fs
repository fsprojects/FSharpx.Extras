module FSharp.Monad.Iteratee.CPS

open System

type Stream<'a> =
  | Chunk of 'a
  | Empty
  | EOF

type IterateeCPS<'el,'a,'r> = Iteratee of (((Stream<'el> -> IterateeCPS<'el,'a,'r>) -> 'r) -> (exn -> 'r) -> ('a * Stream<'el> -> 'r) -> 'r)

let runIter (Iteratee(i)) onCont onError onDone = i onCont onError onDone
let returnI x = Iteratee(fun _ _ onDone -> onDone(x, Empty))
let rec bind (m: IterateeCPS<'el,'a,'r>) (f: 'a -> IterateeCPS<'el,'b,'r>) : IterateeCPS<'el,'b,'r> =
  Iteratee(fun onCont onError onDone ->
    let mdone (a, s) =
      let fcont k = runIter (k s) onCont onError onDone
      match s with
      | Empty -> runIter (f a) onCont onError onDone
      | _ -> runIter (f a) fcont onError (fun (x, _) -> onDone(x, s))
    in runIter m (fun k -> onCont(k >> (fun m' -> bind m' f))) onError mdone)

type IterateeCPSBuilder() =
  member this.Return(x) = returnI x
  member this.ReturnFrom(m:IterateeCPS<_,_,_>) = m
  member this.Bind(m, k) = bind m k
  member this.Zero() = returnI ()
  member this.Combine(comp1, comp2) = bind comp1 (fun () -> comp2)
  member this.Delay(f) = bind (returnI ()) f
let iterateeCPS = IterateeCPSBuilder()

let throw e = Iteratee(fun _ onError _ -> onError e)
let throwRecoverable e i = Iteratee(fun onCont onError _ -> onError e; onCont i)
let doneI x str = Iteratee(fun _ _ onDone -> onDone(x, str))
let contI k = Iteratee(fun onCont _ _ -> onCont k)
let liftI k = contI k
let joinI outer = bind outer (fun inner ->
  Iteratee(fun onCont onError onDone ->
    let od (x, _) = onDone(x, Empty)
    let rec oc k = runIter (k EOF) oc' onError od
    and oc' k = onError(Exception("divergent iteratee"))
    runIter inner oc onError od))

let run i =
  let rec onCont k = runIter (k EOF) onCont' onError onDone
  and onCont' k = Choice1Of2 (Exception("divergent iteratee"))
  and onError e = Choice1Of2 e
  and onDone (x,_) = Choice2Of2 x
  runIter i onCont onError onDone

// This matches the run_ implementation from Iteratee
let run_ i =
  match run i with
  | Choice1Of2 e -> raise e
  | x -> x

let streamToList<'a,'b> : IterateeCPS<'a list,'a list,'b> =
  let rec step acc str =
    match str with
    | Empty | Chunk [] -> liftI (step acc)
    | Chunk ls -> liftI (step (acc @ ls))
    | str -> doneI acc str
  liftI (step [])


(* Enumerators *)
