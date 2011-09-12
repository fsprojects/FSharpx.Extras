// ----------------------------------------------------------------------------
// F# async extensions (StockSlidingWindow.fsx)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

// This example demonstrates how to implement a simple mouse following
// algorithm using asynchronous sequences and Observable.window

#r "..\\bin\\FSharp.AsyncExtensions.dll"

open FSharp.Control
open System.Drawing
open System.Windows.Forms

let form = new Form(Visible=true, TopMost=true)

// Asynchronous sequence that returns cursor position at the 50 FPS rate
let updates = 
  asyncSeq { 
    while true do
      yield form.PointToClient(Cursor.Position)
      do! Async.Sleep(20) }
  |> AsyncSeq.toObservable

// Turn the updates into floating point numbers and calculate average
// over sliding window containing the last 20 values 
updates
|> Observable.map (fun me -> float32 me.X, float32 me.Y)
|> Observable.windowed 20
|> Observable.map (fun win ->
      let x = Array.averageBy fst win
      let y = Array.averageBy snd win
      x, y)
// Draw an ellispe at the calculated location
|> Observable.add (fun (x, y) ->
    use gr = form.CreateGraphics()
    gr.Clear(Color.White)
    gr.FillEllipse(Brushes.DarkOliveGreen, x - 10.0f, y - 10.0f, 20.0f, 20.0f) )
