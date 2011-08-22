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
  Iteratee of (('a -> Stream<'el> -> 'r) -> ((Stream<'el> -> IterateeCPS<'el,'a,'r>) -> exn option -> 'r) -> 'r)

type EnumeratorCPS<'el,'a,'r when 'el : equality> =
  IterateeCPS<'el,'a,IterateeCPS<'el,'a,'r>> -> IterateeCPS<'el,'a,'r>

type EnumerateeCPS<'eli,'elo,'a,'r when 'elo:equality and 'eli:equality> = 
  IterateeCPS<'eli,'a,IterateeCPS<'elo,IterateeCPS<'eli,'a,'r>,'r>> -> IterateeCPS<'elo,IterateeCPS<'eli,'a,'r>,'r>

[<AutoOpen>]
module Primitives =
  let runIter (Iteratee i) onDone onCont = i onDone onCont
  let doneI x str = Iteratee <| fun onDone _ -> onDone x str
  let contI k e = Iteratee <| fun _ onCont -> onCont k e
  let liftI k = Iteratee <| fun _ onCont -> onCont k None
  let rec fmap f m =
    Iteratee <| fun onDone onCont ->
      let od = onDone << f
      let oc k e = onCont (fun s -> fmap f (k s)) e
      runIter m od oc

  let returnI x = Iteratee <| fun onDone _ -> onDone x Empty
  let bind (m: IterateeCPS<'el,'a,'r>) (f: 'a -> IterateeCPS<'el,'b,'r>) : IterateeCPS<'el,'b,'r> =
    let rec inner m f =
      Iteratee <| fun onDone onCont ->
        let mdone a s =
          let fcont k = function
            | None -> runIter (k s) onDone onCont
            | Some e -> onCont k (Some e) 
          in match s with
             | Empty -> runIter (f a) onDone onCont
             | _ -> runIter (f a) (fun x _ -> onDone x s) fcont
        in runIter m mdone (fun k e -> onCont (fun s -> inner (k s) f) e)
    inner m f

  let inline (>>=) m f = bind m f
  let inline (<*>) f m = f >>= fun f' -> fmap f' m

  let run i =
    let rec onDone x _ = x
    and onCont k = function
      | None -> runIter (k (EOF None)) onDone onCont'
      | Some e -> raise e
    and onCont' k = function
      | None -> failwith "divergent iteratee"
      | Some e -> raise e
    in runIter i onDone onCont

  let tryRun i =
    let rec onDone x _ = Choice1Of2 x
    and onCont k = function
      | None -> runIter (k (EOF None)) onDone onCont'
      | Some e -> Choice2Of2 e
    and onCont' k = function
      | None -> Choice2Of2 (Exception("divergent iteratee"))
      | Some e -> Choice2Of2 e
    in runIter i onDone onCont

  let either f g = function
    | Choice1Of2 x -> f x
    | Choice2Of2 y -> g y
    
  let rec lift f i =
    Iteratee <| fun onDone onCont ->
      let od a str = Choice1Of2(a,str)
      let oc k e = Choice2Of2(lift f << k, e)
      f (runIter i od oc) >>= either (fun (a,b) -> onDone a b) (fun (k,e) -> onCont k e)
  
  let rec throw e = contI (fun _ -> throw e) (Some e)
  let throwRecoverable e i = contI i (Some e)
  let rec checkErr i =
    Iteratee <| fun onDone onCont ->
      let od = onDone << Choice2Of2
      let oc k = function
        | None -> onCont (checkErr << k) None
        | Some e -> onDone (Choice1Of2 e) Empty
      runIter i od oc

  let identity<'a when 'a : equality> = doneI () (Empty:Stream<'a>)
  let skipToEof<'a when 'a : equality> =
    let rec loop() =
      let check = function
        | Chunk _ -> loop() 
        | s -> doneI () (s:Stream<'a>)
      in contI check None
    loop ()

  let joinI outer = bind outer (fun inner ->
    Iteratee <| fun onDone onCont ->
      let od x _ = onDone x Empty
      let rec oc k = function
        | None -> runIter (k (EOF None)) od oc'
        | Some e -> runIter (throw e) onDone onCont
      and oc' _ e = runIter (throw (Exception("divergent iteratee"))) onDone onCont
      runIter inner od oc)

//  let enumEOF i =
//    let rec onDone x _ = doneI x (EOF None)
//    and onCont k = function
//      | None -> runIter (k (EOF None)) onDone onCont'
//      | Some e -> contI k e
//    and onCont' k = function
//      | None -> throw (Exception("divergent iteratee"))
//      | Some e -> contI k e
//    in runIter i onDone onCont
//
//  let enumErr e i =
//    let rec onDone x _ = doneI x (EOF (Some e))
//    and onCont k = function
//      | None -> runIter (k (EOF (Some e))) onDone onCont'
//      | Some e' -> contI k e'
//    and onCont' k = function
//      | None -> throw (Exception("divergent iteratee"))
//      | Some e' -> contI k e'
//    in runIter i onDone onCont

type IterateeCPSBuilder() =
  member this.Return(x) = returnI x
  member this.ReturnFrom(m:IterateeCPS<_,_,_>) = m
  member this.Bind(m, k) = bind m k
  member this.Zero() = returnI ()
  member this.Combine(comp1, comp2) = bind comp1 (fun () -> comp2)
  member this.Delay(f) = bind (returnI ()) f
let iterateeCPS = IterateeCPSBuilder()
