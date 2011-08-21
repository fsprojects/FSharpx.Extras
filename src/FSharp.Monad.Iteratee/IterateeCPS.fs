module FSharp.Monad.Iteratee.CPS

open System

[<CustomEquality>]
[<NoComparison>] // TODO: Implement IStructuralComparable
type Stream<'a when 'a : equality> =
  | Chunk of 'a
  | Empty
  | EOF of exn option
  override x.Equals(y) =
    if y.GetType() <> typeof<Stream<_>> then false
    else let y = unbox<Stream<_>> y in
         match x, y with
         | Chunk c1, Chunk c2 -> c1 = c2
         | EOF None, EOF None -> true
         | EOF (Some e1), EOF (Some e2) -> true
         | _ -> false
  override x.GetHashCode() =
    // TODO: Real implementation of GetHashCode()
    match x with
    | Empty -> 0
    | Chunk xs -> xs.GetHashCode()
    | EOF e -> e.GetHashCode()

module Stream =
  let map f = function
    | Chunk xs -> Chunk (f xs)
    | s -> s

type IterateeCPS<'el,'a,'r when 'el : equality> =
  Iteratee of (((Stream<'el> -> IterateeCPS<'el,'a,'r>) -> 'r) -> (exn -> 'r) -> ('a -> Stream<'el> -> 'r) -> 'r)

type EnumeratorCPS<'el,'a,'r when 'el : equality> =
  IterateeCPS<'el,'a,IterateeCPS<'el,'a,'r>> -> IterateeCPS<'el,'a,'r>

type EnumerateeCPS<'eli,'elo,'a,'r when 'elo:equality and 'eli:equality> = 
  IterateeCPS<'eli,'a,IterateeCPS<'elo,IterateeCPS<'eli,'a,'r>,'r>> -> IterateeCPS<'elo,IterateeCPS<'eli,'a,'r>,'r>

[<AutoOpen>]
module Primitives =
  let runIter (Iteratee i) onCont onError onDone = i onCont onError onDone
  let returnI x = Iteratee <| fun _ _ onDone -> onDone x Empty
  let throw e = Iteratee <| fun _ onError _ -> onError e
  let throwRecoverable e i = Iteratee <| fun onCont onError _ -> onError e; onCont i
  let doneI x str = Iteratee <| fun _ _ onDone -> onDone x str
  let contI k = Iteratee <| fun onCont _ _ -> onCont k
  let liftI k = contI k
  let rec fmap f m =
    Iteratee <| fun onCont onError onDone ->
      let od (x,s) = onDone (f x,s)
      let oc k = onCont <| fun s -> fmap f (k s)
      runIter m oc onError od

  let rec bind (m: IterateeCPS<'el,'a,'r>) (f: 'a -> IterateeCPS<'el,'b,'r>) : IterateeCPS<'el,'b,'r> =
    Iteratee <| fun onCont onError onDone ->
      let mdone a s =
        let fcont k = runIter (k s) onCont onError onDone in
        match s with
        | Empty -> runIter (f a) onCont onError onDone
        | _ -> runIter (f a) fcont onError (fun x _ -> onDone x s)
      in runIter m (fun k -> onCont (k >> (fun m' -> bind m' f))) onError mdone

  let inline (>>=) m f = bind m f
  let inline (<*>) f m = f >>= fun f' -> fmap f' m

  let joinI outer = bind outer (fun inner ->
    Iteratee <| fun onCont onError onDone ->
      let od (x, _) = onDone(x, Empty)
      let rec oc k = runIter (k (EOF None)) oc' onError od
      and oc' k = onError (Exception("divergent iteratee"))
      runIter inner oc onError od)

  let catchError h i =
    Iteratee <| fun onCont _ onDone ->
      let rec oc k = runIter (contI (fun s -> (k s))) oc h onDone
      runIter i oc h onDone

  let tryFinally f i =
    Iteratee <| fun onCont onError onDone ->
      let od x = let r = onDone x in f(); r
      let oe e = let r = onError e in f(); r
      let rec oc k = runIter (contI (fun s -> (k s))) oc oe od
      runIter i oc oe od

  let run i =
    let rec onCont k = runIter (k (EOF None)) onCont' onError onDone
    and onCont' k = Choice1Of2 <| Exception("divergent iteratee")
    and onError e = Choice1Of2 e
    and onDone x _ = Choice2Of2 x
    in runIter i onCont onError onDone
  
  // This matches the run_ implementation from Iteratee
  let run_ i =
    match run i with
    | Choice1Of2 e -> raise e
    | x -> x
  
  let enumEOF i =
    let rec onDone x _ = doneI x (EOF None)
    and onError e = throw e
    and onCont k = runIter (k (EOF None)) onCont' onError onDone
    and onCont' k = throw <| failwith "divergent iteratee"
    in runIter i onCont onError onDone

  let enumErr e i =
    let rec onDone x _ = doneI x (EOF (Some e))
    and onError e' = throw e'
    and onCont k = runIter (k (EOF (Some e))) onCont' onError onDone
    and onCont' k = throw <| failwith "divergent iteratee"
    in runIter i onCont onError onDone

type IterateeCPSBuilder() =
  member this.Return(x) = returnI x
  member this.ReturnFrom(m:IterateeCPS<_,_,_>) = m
  member this.Bind(m, k) = bind m k
  member this.Zero() = returnI ()
  member this.Combine(comp1, comp2) = bind comp1 (fun () -> comp2)
  member this.Delay(f) = bind (returnI ()) f
  member this.TryWith(m, h) = catchError h m
  member this.TryFinally(m, compensation) = tryFinally compensation m
  member this.Using(res:#IDisposable, body) =
    this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))
  member this.While(guard, m) =
    if not(guard()) then this.Zero() else
      this.Bind(m, (fun () -> this.While(guard, m)))
  member this.For(sequence:#seq<_>, body) =
    this.Using(sequence.GetEnumerator(),
               (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
let iterateeCPS = IterateeCPSBuilder()

module List =

  let enumPure1Chunk str i =
    let rec onDone a str' =
      match str' with
      | Chunk str' -> doneI a (Chunk (str @ str'))
      | _ -> doneI a (Chunk str)
    and onCont k = runIter (k (Chunk str)) onCont onError onDone
    and onError e = throw e
    in runIter i onCont onError onDone

  let enumChunk = function
    | Empty -> enumPure1Chunk []
    | Chunk xs -> enumPure1Chunk xs
    | EOF None -> enumEOF
    | EOF (Some e) -> enumErr e

//  let rec enumList input i =
//    match input with
//    | [] -> i
//    | _ -> let rec onCont l k =
//             match l with
//             | x::xs -> enumList xs <| k (Chunk x)
//             | _ -> contI k
//           in runIter i (onCont input) throw doneI

  let streamToList<'a,'b when 'a : equality> : IterateeCPS<'a list,'a list,'b> =
    let rec step acc str =
      match str with
      | Empty | Chunk [] -> liftI (step acc)
      | Chunk ls -> liftI (step (acc @ ls))
      | str -> doneI acc str
    liftI (step [])
  