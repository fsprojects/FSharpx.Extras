module FSharp.Monad.Iteratee.CPS

open System

type Stream<'a> =
  | Chunk of 'a
  | Empty
  | EOF

type IterateeCPS<'el,'a,'r> = Iteratee of (((Stream<'el> -> IterateeCPS<'el,'a,'r>) * exn option -> 'r) -> ('a * Stream<'el> -> 'r) -> 'r)

let runIter (Iteratee(i)) onCont onDone = i onCont onDone
let returnI x = Iteratee(fun _ onDone -> onDone(x, Empty))
let rec bind (m: IterateeCPS<'el,'a,'r>) (f: 'a -> IterateeCPS<'el,'b,'r>) : IterateeCPS<'el,'b,'r> =
  Iteratee(fun onCont onDone ->
    let mdone (a, s) =
      let fcont (k, e) =
        match e with
        | None -> runIter (k s) onCont onDone
        | Some _ -> onCont(k, e)
      match s with
      | Empty -> runIter (f a) onCont onDone
      | _ -> runIter (f a) fcont (fun (x, _) -> onDone(x, s))
    in runIter m (fun (k, e) -> onCont(k >> (fun m' -> bind m' f), e)) mdone)

type IterateeCPSBuilder() =
  member this.Return(x) = returnI x
  member this.ReturnFrom(m:IterateeCPS<_,_,_>) = m
  member this.Bind(m, k) = bind m k
  member this.Zero() = returnI ()
  member this.Combine(comp1, comp2) = bind comp1 (fun () -> comp2)
  member this.Delay(f) = bind (returnI ()) f
let iterateeCPS = IterateeCPSBuilder()

let rec throw e = Iteratee(fun onCont _ -> onCont((fun _ -> throw e), (Some e)))
let throwRecoverable e i = Iteratee(fun onCont _ -> onCont(i, (Some e)))
let doneI x str = Iteratee(fun _ onDone -> onDone(x, str))
let contI k e = Iteratee(fun onCont _ -> onCont(k, e))
let liftI k = Iteratee(fun onCont _ -> onCont(k, None))
let joinI outer = bind outer (fun inner ->
  Iteratee(fun onCont onDone ->
    let od (x, _) = onDone(x, Empty)
    let rec oc = function
      | k, None -> runIter (k EOF) oc' od
      | _, Some e -> runIter (throw e) onCont od
    and oc' (_, e) = runIter (throw (Option.get e)) oc od
    runIter inner oc od))
