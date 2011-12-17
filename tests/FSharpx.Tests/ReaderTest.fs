module FSharpx.Tests.ReaderTest

open System
open System.Threading
open FSharpx.Reader
open NUnit.Framework
open FsUnit

// Basic monadic builder tests.
[<Test>]
let ``Return should enable return``() =
  let expected = 1
  let r = reader_ {
    return expected }
  r 0 |> should equal expected

[<Test>]
let ``ReturnFrom should enable return!``() =
  let expected = 1
  let r = reader_ {
    return! ask_ }
  r expected |> should equal expected

[<Test>]
let ``ReturnFrom should enable return! from asks``() =
  let expected = 1
  let r = reader_ {
    return! asks (fun i -> i + 1) }
  r 0 |> should equal expected

[<Test>]
let ``Bind should enable let!``() =
  let expected = 1
  let r = reader_ {
    let! env = ask_
    return env }
  r expected |> should equal expected

[<Test>]
let ``Zero should allow no else branch``() =
  let called = ref false
  let r = reader_ {
    if false then
      called := true }
  r 1
  !called |> should be False

[<Test>]
let ``Combine should combine if statement``() =
  let expected = 0
  let r = reader_ {
    let! x = ask_
    if true then ()
    return x }
  r expected |> should equal expected

[<Test>]
let ``TryWith should catch exception``() =
  let called = ref false
  let r = reader_ {
    try failwith "FAIL"
    with e -> called := true }
  r ()
  !called |> should be True

[<Test>]
let ``TryFinally with exception should execute finally``() =
  let called = ref false
  let r = reader_ {
    try failwith "FAIL"
    finally called := true }
  try r ()
  with e -> ()
  !called |> should be True

[<Test>]
let ``Using should call Dispose``() =
  let disposed = ref false
  let disposable =
    { new IDisposable with
        member __.Dispose() = disposed := true }
  let r = reader_ {
    use d = disposable
    () }
  r ()
  !disposed |> should be True

[<Test>]
let ``use! should call Dispose``() =
  let disposed = ref false
  let disposable = reader_ {
    return { new IDisposable with
               member __.Dispose() = disposed := true } }
  let r = reader_ {
    use! d = disposable
    () }
  r ()
  !disposed |> should be True

[<Test>]
let ``while should increment count``() =
  let count = ref 0
  let r = reader_ {
    while !count < 3 do
      incr count }
  r ()
  !count |> should equal 3

[<Test>]
let ``for should increment count``() =
  let count = ref 0
  let r = reader_ {
    for i = 0 to 1 do
      incr count }
  r ()
  !count |> should equal 2
