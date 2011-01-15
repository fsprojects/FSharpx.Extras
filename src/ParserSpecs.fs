module ParserSpecs
open System
open System.Threading
open FSharp.Monad.Parser
open NUnit.Framework
open BaseSpecs

let inline (==) expected comp =
  isTrue(match run (comp expected) with
         | Some(v, _) -> v = expected
         | _ -> false)
let inline (!=) expected comp =
  isFalse(match run (comp expected) with
          | Some(v, _) -> v = expected
          | _ -> false)

// Basic monadic builder tests.
[<Test>]
let ``Return should enable return``() =
  let expected = 1
  let r = parse {
    return expected }
  expected == r

[<Test>]
let ``ReturnFrom should enable return!``() =
  let expected = 1
  let r = parse {
    return! parse { return expected } }
  expected == r

[<Test>]
let ``Bind should enable let!``() =
  let expected = 1
  let r = parse {
    let! x = parse { return expected }
    return x }
  expected == r

[<Test>]
let ``Zero should allow no else branch``() =
  let called = ref false
  let r = parse {
    if false then
      called := true }
  let res = run (r 1)
  isFalse !called

[<Test>]
let ``Combine should combine if statement``() =
  let expected = 0
  let r = parse {
    let! x = parse { return expected }
    if true then ()
    return x }
  expected == r

[<Test>]
let ``TryWith should catch exception``() =
  let called = ref false
  let r = parse {
    try failwith "FAIL"
    with e -> called := true }
  run (r ()) |> ignore
  isTrue !called

[<Test>]
let ``TryFinally with exception should execute finally``() =
  let called = ref false
  let r = parse {
    try failwith "FAIL"
    finally called := true }
  try run (r ()) |> ignore
  with e -> ()
  isTrue !called

[<Test>]
let ``Using should call Dispose``() =
  let disposed = ref false
  let disposable =
    { new IDisposable with
        member __.Dispose() = disposed := true }

  let r = parse {
    use d = disposable
    () }

  run (r ()) |> ignore
  isTrue !disposed

[<Test>]
let ``use! should call Dispose``() =
  let disposed = ref false
  let disposable = parse {
    return { new IDisposable with
               member __.Dispose() = disposed := true } }

  let r = parse {
    use! d = disposable
    () }

  run (r ()) |> ignore
  isTrue !disposed

//[<Test>]
//let ``while should increment count``() =
//  let count = ref 0
//  let r = parse {
//    while !count < 3 do
//      incr count }
//  run (r ()) |> ignore
//  Assert.AreEqual(3, !count)
//
//[<Test>]
//let ``for should increment count``() =
//  let count = ref 0
//  let r = parse {
//    for i = 0 to 2 do
//      incr count }
//  run (r ()) |> ignore
//  Assert.AreEqual(3, !count)

// Char parser tests
let tokens = [0uy..127uy]
let ``A-Z`` = [65uy..90uy]
let ``a-z`` = [97uy..122uy]
let matchToken ts t = ts |> List.exists (fun x -> x = t)

let pchar pattern = fun chr -> async {
  match chr with
  | c when matchToken pattern c -> return Some(c, chr)
  | _ -> return None }

let token = pchar tokens
let upper = pchar ``A-Z``
let lower = pchar ``a-z``

[<Test>]
let ``Should match 'a' as a token``() =
  let expected = 'a'B
  expected == token

[<Test>]
let ``Should match 'a' as a lower``() =
  let expected = 'a'B
  expected == lower

[<Test>]
let ``Should not match 'a' as an upper``() =
  let expected = 'a'B
  expected != upper

[<Test>]
let ``Should match '\r' as a token``() =
  let expected = '\r'B
  expected == token

[<Test>]
let ``Should not match '\r' as a lower``() =
  let expected = '\r'B
  expected != lower

[<Test>]
let ``Should not match '\r' as an upper``() =
  let expected = '\r'B
  expected != upper

[<Test>]
let ``Should match '\n' as a token``() =
  let expected = '\n'B
  expected == token

[<Test>]
let ``Should not match '\n' as a lower``() =
  let expected = '\n'B
  expected != lower

[<Test>]
let ``Should not match '\n' as an upper``() =
  let expected = '\n'B
  expected != upper