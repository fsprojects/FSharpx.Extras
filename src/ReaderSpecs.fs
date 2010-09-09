module ReaderTests
open System
open System.Threading
open FSharp.Monad.Reader
open NaturalSpec

// Basic monadic builder tests.
[<Scenario>]
let ``Return should enable return``() =
  let expected = 1
  let r = reader {
    return expected }
  Given 0
  |> When (runReader r)
  |> It should equal expected
  |> Verify

[<Scenario>]
let ``ReturnFrom should enable return!``() =
  let expected = 1
  let r = reader {
    return! ask }
  Given expected
  |> When (runReader r)
  |> It should equal expected
  |> Verify

[<Scenario>]
let ``ReturnFrom should enable return! from asks``() =
  let expected = 1
  let r = reader {
    return! asks (fun i -> i + 1) }
  Given 0
  |> When (runReader r)
  |> It should equal expected
  |> Verify

[<Scenario>]
let ``Bind should enable let!``() =
  let expected = 1
  let r = reader {
    let! env = ask
    return env }
  Given expected
  |> When (runReader r)
  |> It should equal expected
  |> Verify

//[<Scenario>]
//let ``Zero should allow no else branch``() =
//  let called = ref false
//  let r = reader {
//    if false then
//      called := true }
//
//  runReader r 1
//  isFalse !called

[<Scenario>]
let ``Combine should combine if statement``() =
  let expected = 0
  let r = reader {
    let! x = ask
    if true then ()
    return x }
  Given expected
  |> When (runReader r)
  |> It should equal expected
  |> Verify

//[<Scenario>]
//let ``TryWith should catch exception``() =
//  let called = ref false
//  let r = reader {
//    try failwith "FAIL"
//    with e -> called := true }
//
//  runReader r ()
//  isTrue !called
//
//[<Scenario>]
//let ``TryFinally with exception should execute finally``() =
//  let called = ref false
//  let r = reader {
//    try failwith "FAIL"
//    finally called := true }
//
//  runReader r ()
//  isTrue !called
//
//[<Scenario>]
//let ``Using should call Dispose``() =
//  let disposed = ref false
//  let disposable =
//    { new IDisposable with
//        member __.Dispose() = disposed := true }
//
//  let r = reader {
//    use d = disposable
//    () }
//
//  runReader r ()
//  isTrue !disposed
//
//[<Scenario>]
//let ``use! should call Dispose``() =
//  let disposed = ref false
//  let disposable = reader {
//    return { new IDisposable with
//               member __.Dispose() = disposed := true } }
//
//  let r = reader {
//    use! d = disposable
//    () }
//
//  runReader r ()
//  isTrue !disposed
//
//[<Scenario>]
//let ``while should increment count``() =
//  let count = ref 0
//  let r = reader {
//    while !count < 1 do
//      incr count }
//
//  runReader r ()
//  areEqual 1 !count
//
//[<Scenario>]
//let ``for should increment count``() =
//  let count = ref 0
//  let r = reader {
//    for i = 0 to 1 do
//      incr count }
//
//  runReader r ()
//  areEqual 1 !count

//// Using Locks
//let tryRunLock lock m =
//  let lock = box lock
//  let lockToken = ref false
//  Monitor.Enter(lock, lockToken)
//
//  match !lockToken with
//  | true -> try Some(runReader m lock)
//            finally Monitor.Exit lock
//  | _ -> None
//
//let pulseAll = Reader Monitor.PulseAll
//let wait = Reader ((Monitor.Wait:obj -> bool) >> ignore)
//
//// Using tryRunLock
//open System.Collections.Generic
//
//let pop (stack:Stack<_>) = reader {
//  while stack.Count = 0 do return! wait
//  return stack.Pop() }
//
//let push (stack:Stack<_>) x = reader {
//  if stack.Count = 0 then return! pulseAll
//  do stack.Push(x) }
//
//let lockObj = new obj()
//let move s1 s2 =
//  reader {
//    let! x = pop s1
//    do! push s2 x
//    return x }
//  |> tryRunLock lockObj
//
//[<Scenario>]
//let ``Can pop and push between stacks``() =
//  let s1 = new Stack<int>([1..3])
//  let s2 = new Stack<int>()
//
//  let moved = move s1 s2
//
//  Assert.Equal(Some(3), moved)

//// WatiN F# extensions
//open WatiN.Core
//
//let openPage (url:string) = reader {
//  return! asks (fun (browser:Browser) -> browser.GoTo url) }
//
//let enterText (textBox:string) (text:string) = reader {
//  return! asks (fun (browser:Browser) -> browser.TextField(Find.ByName(textBox)).TypeText text) }
//
//let containsText (text : string) = reader {
//  return! asks (fun (browser : Browser) -> browser.ContainsText text) }
// 
//let clickButton (buttonText : string) = reader {
//  return! asks (fun (browser : Browser) -> browser.Button(Find.ByName(buttonText)).Click()) }
//
//let closePage = reader {
//  return! asks (fun (browser : Browser) -> browser.Dispose()) }
//
//let runScript (script:Reader<Browser,'a>) =
//  runReader script (new IE() :> Browser)
//
//// WatiN tests
//[<Scenario>]
//let ``Can find CodeBetter on Bing``() =
//  reader {
//    do! openPage "http://bing.com"
//    do! enterText "q" "CodeBetter"
//    do! clickButton "go"
//    let! result = containsText "CodeBetter"
//    isTrue result
//    do! closePage } |> runScript