module FSharpx.Tests.UndoTest

open FSharpx.Functional.Undo
open NUnit.Framework
open FsUnit

// Simple "text editor" example
let addText text = combineWithCurrent (+) text

[<Test>]
let ``When starting a text editior with empty string, it should have a empty string in history``() =
  newHistory ""
   |> addText ""
   |> snd 
   |> current 
   |> should equal ""

[<Test>]
let ``When starting a text editor with "" and adding two strings, it should contain both string``() =
  let test = undoable {
    let! _ = addText "foo"
    let! _ = addText "bar"
    return () }
  let actual = exec test (newHistory "")
  actual |> should equal "foobar"

[<Test>]
let ``When starting a text editor with "" and adding three strings and undoing two, it should contain the first string``() =
  let test = undoable {
    let! _ = addText "foo"
    let! _ = addText "bar"
    let! _ = addText "baz"
    let! firstUndo = undo
    let! secondUndo = undo
    return firstUndo,secondUndo }
  let actual = test (newHistory "")
  actual |> snd |> current |> should equal "foo"
  actual |> fst |> should equal (true,true)

[<Test>]
let ``When starting a text editor with "" and adding three strings and undoing two and redoing two, it should contain all three strings``() =
  let test = undoable {
    let! _ = addText "foo"
    let! _ = addText "bar"
    let! _ = addText "baz"
    let! _ = undo
    let! _ = undo
    let! firstRedo = redo
    let! secondRedo = redo
    return firstRedo,secondRedo }
  let actual = test (newHistory "")
  actual |> snd |> current |> should equal "foobarbaz"
  actual |> fst |> should equal (true,true)

[<Test>]
let ``When starting a text editor with "" and adding a string, it should allow two undos``() =
  let test = undoable {
    let! _ = addText "foo"    
    let! firstUndo = undo
    let! secondUndo = undo
    return firstUndo,secondUndo }
  let actual = test (newHistory "")
  actual |> snd |> current |> should equal ""
  actual |> fst |> should equal (true,true)

[<Test>]
let ``When starting a text editor with "" and adding a string, it should not allow three undos``() =
  let test = undoable {
    let! _ = addText "foo"    
    let! firstUndo = undo
    let! secondUndo = undo
    let! thirdUndo = undo
    return firstUndo,secondUndo,thirdUndo }
  let actual = test (newHistory "")
  actual |> snd |> current |> should equal ""
  actual |> fst |> should equal (true,true,false)

[<Test>]
let ``When starting a text editor with "" and adding a string, it should not allow a redo without an undo``() =
  let test = undoable {
    let! _ = addText "foo"    
    let! firstRedo = redo   
    return firstRedo }
  let actual = test (newHistory "")
  actual |> snd |> current |> should equal "foo"
  actual |> fst |> should equal false