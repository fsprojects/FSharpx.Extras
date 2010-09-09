module StateTests
open FSharp.Monad.State
open NaturalSpec

// Simple example
let tick = state {
  let! n = getState
  do! putState (n + 1)
  return n }

//// Stack example
//let enqueue a = State (fun s -> ((), s @ a::[]))
//let dequeue = State (fun (hd::tl) -> (hd, tl))
//
//let workflow = state {
//  let! queue = getState
//  do! enqueue 4
//  let! hd = dequeue
//  do! enqueue (hd * 3)
//  return hd }

//// WatiN F# extensions
//open WatiN.Core
//open Xunit
//
//let openPage (url:string) = state {
//  let! (browser : Browser) = getState
//  return browser.GoTo(url) }
//
//let enterText (textBox:string) (text:string) = state {
//  let! (browser:Browser) = getState
//  return browser.TextField(Find.ByName(textBox)).TypeText(text) }
//
//let containsText (text : string) = state {
//  let! (browser : Browser) = getState
//  return browser.ContainsText(text) }
// 
//let clickButton (buttonText : string) = state {
//  let! (browser : Browser) = getState
//  return browser.Button(Find.ByName(buttonText)).Click() }
//
//let closePage = state {
//  let! (browser : Browser) = getState
//  return browser.Dispose() }
//
//let runScript (script:State<'a, Browser>) =
//  eval script (new IE() :> Browser)
//
//let isTrue = Assert.True
//
//// WatiN tests
//[<Fact>]
//let ``Can find CodeBetter on Bing``() =
//  runScript (
//    state {
//      do! openPage "http://bing.com"
//      do! enterText "q" "CodeBetter"
//      do! clickButton "go"
//      let! result = containsText "CodeBetter"
//      isTrue result
//      do! closePage })