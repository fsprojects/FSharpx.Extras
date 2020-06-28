module FSharpx.Tests.ReaderTest

open System
open System.Threading
open FSharpx
open FSharpx.Functional
open FSharpx.Reader
open NUnit.Framework
open FsUnitTyped
open TestHelpers

// Basic monadic builder tests.
[<Test>]
let ``Return should enable return``() =
  let expected = 1
  let r = reader {
    return expected }
  r 0 |> shouldEqual expected

[<Test>]
let ``ReturnFrom should enable return!``() =
  let expected = 1
  let r = reader {
    return! ask }
  r expected |> shouldEqual expected

[<Test>]
let ``ReturnFrom should enable return! from asks``() =
  let expected = 1
  let r = reader {
    return! asks (fun i -> i + 1) }
  r 0 |> shouldEqual expected

[<Test>]
let ``Bind should enable let!``() =
  let expected = 1
  let r = reader {
    let! env = ask
    return env }
  r expected |> shouldEqual expected

[<Test>]
let ``Zero should allow no else branch``() =
  let mutable called = false
  let r = reader {
    if false then
      called <- true }
  r 1
  called |> shouldEqual false

[<Test>]
let ``Combine should combine if statement``() =
  let expected = 0
  let r = reader {
    let! x = ask
    if true then ()
    return x }
  r expected |> shouldEqual expected

[<Test>]
let ``TryWith should catch exception``() =
  let mutable called = false
  let r = reader {
    try failwith "FAIL"
    with e -> called <- true }
  r ()
  called |> shouldEqual true

[<Test>]
let ``TryFinally with exception should execute finally``() =
  let mutable called = false
  let r = reader {
    try failwith "FAIL"
    finally called <- true }
  try r ()
  with e -> ()
  called |> shouldEqual true

[<Test>]
let ``Using should call Dispose``() =
  let mutable disposed = false
  let disposable =
    { new IDisposable with
        member __.Dispose() = disposed <- true }
  let r = reader {
    use d = disposable
    () }
  r ()
  disposed |> shouldEqual true

[<Test>]
let ``use! should call Dispose``() =
  let mutable disposed = false
  let disposable = reader {
    return { new IDisposable with
               member __.Dispose() = disposed <- true } }
  let r = reader {
    use! d = disposable
    () }
  r ()
  disposed |> shouldEqual true

[<Test>]
let ``while should increment count``() =
  let mutable count = 0
  let r = reader {
    while count < 3 do
      count <- count + 1 }
  r ()
  count |> shouldEqual 3

[<Test>]
let ``for should increment count``() =
  let mutable count = 0
  let r = reader {
    for i = 0 to 1 do
      count <- count + 1 }
  r ()
  !count |> shouldEqual 2

[<Test>]
let ``use should dispose underlying IDisposable``() =
  let disposeChecker = new DisposeChecker()
  let r =
     (reader {
       use! x = reader {return disposeChecker}
       return x.Disposed
     })()
  Assert.Multiple
    (fun () ->
      disposeChecker.Disposed |> shouldEqual true
      r |> shouldEqual false
    )