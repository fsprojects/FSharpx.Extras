module FSharp.Monad.Tests.UndoTest

open FSharp.Monad.Undo
open NUnit.Framework
open FsUnit

// Simple "text editor" example
let addText newText = undoable {
    let! text = getCurrent()
    do! putToHistory (text + newText) 
    return text}

[<Test>]
let ``When starting a text editior with empty string, it should have a empty string in history``() =
  let actual = addText "" (empty "")
  fst actual |> should equal ""

[<Test>]
let ``When starting a text editor with "" and adding two strings, it should contain both string``() =
  let test = undoable {
    let! _ = addText "foo"
    let! _ = addText "bar"
    return () }
  let actual = exec test (empty "")
  actual |> should equal "foobar"

[<Test>]
let ``When starting a text editor with "" and adding three strings and undoing two, it should contain the first string``() =
  let test = undoable {
    let! _ = addText "foo"
    let! _ = addText "bar"
    let! _ = addText "baz"
    let! _ = undo()
    let! _ = undo()
    return () }
  let actual = exec test (empty "")
  actual |> should equal "foo"

[<Test>]
let ``When starting a text editor with "" and adding three strings and undoing two and redoing two, it should contain all three strings``() =
  let test = undoable {
    let! _ = addText "foo"
    let! _ = addText "bar"
    let! _ = addText "baz"
    let! _ = undo()
    let! _ = undo()
    let! _ = redo()
    let! _ = redo()
    return () }
  let actual = exec test (empty "")
  actual |> should equal "foobarbaz"