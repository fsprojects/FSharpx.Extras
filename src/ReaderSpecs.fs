module ReaderSpecs
open System
open System.Threading
open FSharp.Monad
open NUnit.Framework
open BaseSpecs

// Basic monadic builder tests.
[<Test>]
let ``Return should enable return``() =
  let expected = 1
  let r = reader {
    return expected }
  expected == (runReader r 0)

[<Test>]
let ``ReturnFrom should enable return!``() =
  let expected = 1
  let r = reader {
    return! ask }
  expected == (runReader r expected)

[<Test>]
let ``ReturnFrom should enable return! from asks``() =
  let expected = 1
  let r = reader {
    return! asks (fun i -> i + 1) }
  expected == (runReader r 0)

[<Test>]
let ``Bind should enable let!``() =
  let expected = 1
  let r = reader {
    let! env = ask
    return env }
  expected == (runReader r expected)

[<Test>]
let ``Zero should allow no else branch``() =
  let called = ref false
  let r = reader {
    if false then
      called := true }
  runReader r 1
  isFalse !called

[<Test>]
let ``Combine should combine if statement``() =
  let expected = 0
  let r = reader {
    let! x = ask
    if true then ()
    return x }
  expected == (runReader r expected)

[<Test>]
let ``TryWith should catch exception``() =
  let called = ref false
  let r = reader {
    try failwith "FAIL"
    with e -> called := true }
  runReader r ()
  isTrue !called

[<Test>]
let ``TryFinally with exception should execute finally``() =
  let called = ref false
  let r = reader {
    try failwith "FAIL"
    finally called := true }
  try runReader r ()
  with e -> ()
  isTrue !called

[<Test>]
let ``Using should call Dispose``() =
  let disposed = ref false
  let disposable =
    { new IDisposable with
        member __.Dispose() = disposed := true }

  let r = reader {
    use d = disposable
    () }

  runReader r ()
  isTrue !disposed

[<Test>]
let ``use! should call Dispose``() =
  let disposed = ref false
  let disposable = reader {
    return { new IDisposable with
               member __.Dispose() = disposed := true } }

  let r = reader {
    use! d = disposable
    () }

  runReader r ()
  isTrue !disposed

[<Test>]
let ``while should increment count``() =
  let count = ref 0
  let r = reader {
    while !count < 3 do
      incr count }
  runReader r ()
  3 == !count

[<Test>]
let ``for should increment count``() =
  let count = ref 0
  let r = reader {
    for i = 0 to 1 do
      incr count }
  runReader r ()
  2 == !count
