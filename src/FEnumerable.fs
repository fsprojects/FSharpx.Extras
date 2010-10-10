namespace FSharp.Monad.MinLinq

/// The Iterator monad
type FEnumerable<'a> = unit -> (unit -> 'a option)

[<AutoOpen>]
module FEnumerable =
  open System

  let empty = fun () -> (fun () -> None)

  type FEnumerableBuilder() =
    member this.Zero() = empty
    member this.Return(a) =
      fun () -> let i = ref 0
                (fun () -> if !i = 0 then
                             incr i
                             Some(a)
                           else None)
    member this.ReturnFrom(m: FEnumerable<_>) = m
    member this.Bind(m:FEnumerable<_>, k) = fun () ->
      let e = m()
      (fun () ->
        let rec outerLoop (lastInner:'b option) =
          if lastInner.IsNone then innerLoop lastInner
          else lastInner
        and innerLoop lastInner =
          let lastOuter = e()
          let innerE = match lastOuter with
                       | Some(x) -> Some(k x ())
                       | _       -> None
          match innerE with
          | Some(f) -> let lastInner = f()
                       match lastInner with
                       | None -> outerLoop lastInner
                       | _    -> lastInner
          | _       -> None
        outerLoop None)

    member this.TryWith(m:FEnumerable<_>, h) =
      (fun k -> try m k
                with e -> (h e) k)

    member this.TryFinally(m:FEnumerable<_>, compensation) =
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

  let fenumerable = FEnumerableBuilder()

  let unfold generator seed = fun () ->
    let next = ref seed
    (fun () ->
      let res = generator(!next)
      match res with
      | Some(x,y) -> next := y
                     Some(x)
      | _         -> None )

  let fold f seed m =
    let e = m()
    let rec loop value acc =
      match value with
      | Some(x) -> loop (e()) (f acc x)
      | _       -> acc
    loop (e()) seed 

  let filter m f = fenumerable {
    let! t = m
    if f t then return t
    else return! empty }

  let map m f = fenumerable {
    let! t = m
    return f t } 

  let range (from,length) = unfold (fun x -> if x < from + length then Some(x,x+1) else None) from

  let sum m = fold (fun sum x -> sum + x) 0 m
