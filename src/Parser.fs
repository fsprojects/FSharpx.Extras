namespace FSharp.Monad
module Parser =
  open System

  type Parser<'r, 's> = 's -> Async<('r * 's) option>

  let run = Async.RunSynchronously
  let runParallel comp = comp |> Async.Parallel |> Async.RunSynchronously
  let start = Async.Start
  let continueWith = Async.StartWithContinuations

  let zero : Parser<'r,'s> = fun input -> async { return None }

  let puree v : Parser<'r,'s> = fun input -> async { return Some(v, input) }
    
  let bind (p: Parser<'a,'s>) (f: 'a -> Parser<'b,'s>) : Parser<'b,'s> =
    fun input -> async {
      let! comp = p input
      return! match comp with
              | Some(value, input) -> f value input
              | None -> async { return None } }
  let (>>=) = bind

  let choice (p: Parser<'a,'s>) (q: Parser<'a,'s>) : Parser<'a,'s> =
    fun input -> async {
      let! comp = p input
      return! match comp with
              | Some(v) -> async { return Some(v) }
              | None -> q input }
  let (+++) = choice

  type ParserBuilder() =
    member x.Delay(f) = fun input -> f () input

    member x.Zero() = zero

    member x.Return(v) = puree v

    member x.ReturnFrom(p) : Parser<'r,'s> = p

    member x.Bind(p, f) = p >>= f

    member x.Combine(p, q) = p +++ q

    member this.TryWith(p:Parser<'r,'s>, h) : Parser<'r,'s> =
      fun input -> async {
        try return! p input
        with e -> return! h e input }

    member this.TryFinally(p:Parser<'r,'s>, compensation) : Parser<'r,'s> =
      fun input -> async {
        try return! p input
        finally compensation() }

    member this.Using(res:#IDisposable, body) =
      this.TryFinally(body res, (fun () -> match res with null -> () | disp -> disp.Dispose()))

    member this.While(guard, m) =
      if not(guard()) then zero else
        m >>= (fun () -> this.While(guard, m))

    member this.For(sequence:seq<_>, body) =
      this.Using(sequence.GetEnumerator(),
                 (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

    member x.Yield(v) = puree v

    member x.YieldFrom(p) : Parser<'r,'s> = p
  
  let parse = ParserBuilder()

  let map p f = parse {
    let! x = p
    return f x }

  let (<*>) f a = parse {
    let! f' = f
    let! a' = a
    return f' a' }