module FSharp.Monad.MinLinq.FObservable

open System

/// The Observable monad
type FObservable<'a> = ('a option -> unit) -> unit

let empty = fun o -> o(None)

type FObservableBuilder() =
  member this.Zero() = empty
  member this.Return(a) = fun o -> o(Some(a)); o(None)
  member this.ReturnFrom(m:FObservable<_>) = m
  member this.Bind(m:FObservable<'a>, k:'a -> FObservable<'b>) =
    fun o -> m(fun x -> match x with
                        | Some(x) -> k x (fun y -> if y.IsSome then (o y)) 
                        | _       -> o None)

  member this.TryWith(m:FObservable<_>, h) =
    (fun k -> try m k
              with e -> (h e) k)

  member this.TryFinally(m:FObservable<_>, compensation) =
    (fun k -> try m k
              finally compensation())

  member this.Using(res:#IDisposable, body) =
    this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))

  member this.Combine(comp1, comp2) = this.Bind(comp1, fun () -> comp2)

  member this.Delay(f) = this.Bind(this.Return(), f)

  member this.While(guard, m) =
    if not(guard()) then this.Zero() else
      this.Bind(m, (fun () -> this.While(guard, m)))

  member this.For(sequence:seq<_>, body) =
    this.Using(sequence.GetEnumerator(),
               (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

let fobservable = FObservableBuilder()

let unfold generator seed = fun o ->
  let rec loop t =
    let res = generator t
    match res with
    | Some(x,y) -> o(Some(x))
                   loop y
    | None      -> o(None) 
  loop seed

let fold f seed m =
  let result = ref seed
  let stop = ref false
  m(fun x -> match x with
             | Some(x) when not (!stop) -> result := f !result x
             | _                        -> stop := true )
  !result

let filter m f = fobservable {
  let! t = m
  if f t then return t
  else return! empty }

let map m f = fobservable {
  let! t = m
  return f t } 

let range (from,length) = unfold (fun x -> if x < from + length then Some(x,x+1) else None) from

let sum m = fold (fun sum x -> sum + x) 0 m
  