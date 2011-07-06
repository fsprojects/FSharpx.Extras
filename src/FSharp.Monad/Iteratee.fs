module FSharp.Monad.Iteratee

open System.IO

type Stream<'el> =
  | Chunk of 'el
  | Empty
  | EOF

type Iteratee<'el,'acc> =
  | Continue of exn option * (Stream<'el> -> Iteratee<'el,'acc> * Stream<'el>)
  | Yield of 'acc

type Enumerator<'el,'acc> = Iteratee<'el,'acc> -> Iteratee<'el,'acc>

type Enumeratee<'elo,'eli,'acc> = Iteratee<'eli,'acc> -> Iteratee<'elo,Iteratee<'eli,'acc>>

let runIteratee m =
  match m with
  | Yield x -> x
  | Continue (Some e, _) -> raise e
  | Continue (_, k) ->
      match fst (k EOF) with
      | Yield xx   -> xx
      | Continue _ -> failwith "Diverging iteratee"

let rec bind m f =
  match m with
  | Yield x -> f x
  | Continue (e, k) ->
      Continue(e, fun s ->
        match k s with
        | Yield x, s' ->
            match f x with
            | Continue (None, k) -> k s'
            | i                  -> i,s'
        | m', s'        -> bind m' f, s')

type IterateeBuilder() =
  member this.Return(x) = Yield x
  member this.ReturnFrom(m:Iteratee<_,_>) = m
  member this.Bind(m, k) = bind m k
  member this.Zero() = Yield ()
  member this.Combine(comp1, comp2) = bind comp1 (fun () -> comp2)
  member this.Delay(f) = bind (Yield()) f
let iteratee = IterateeBuilder()

module Operators =
  open FSharp.Monad.Operators

  let inline returnM x = returnM iteratee x
  let inline (>>=) m f = bindM iteratee m f
  let inline (<*>) f m = applyM iteratee iteratee f m
  let inline lift f m = liftM iteratee f m
  let inline (<!>) f m = lift f m
  let inline lift2 f a b = returnM f <*> a <*> b
  let inline ( *>) x y = lift2 (fun _ z -> z) x y
  let inline ( <*) x y = lift2 (fun z _ -> z) x y
  let inline (>>.) m f = bindM iteratee m (fun _ -> f)
