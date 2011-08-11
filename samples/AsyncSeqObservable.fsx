// ----------------------------------------------------------------------------
// F# async extensions (AsyncSeqObservable.fsx)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

// This example demonstrates how to convert IObservable<'T> to AsyncSeq<'T>

#r "..\\bin\\FSharp.AsyncExtensions.dll"
open FSharp.Control
open System.Windows.Forms
open System.Threading

// Create simple winforms user interface with a button and multiline text box
let frm = new Form(Visible=true, TopMost=true, Width=440)
let btn = new Button(Left=10, Top=10, Width=150, Text="Async Operation")
let out = new TextBox(Left=10, Top=40, Width=400, Height=200, Multiline=true)
frm.Controls.Add(btn)
frm.Controls.Add(out)

// Prints message to the displayed text box
let wprint fmt = 
  Printf.kprintf (fun s -> out.Text <- out.Text + s) fmt


// The sample demonstrates two ways of converting IObservable<_> values to 
// asynchronous sequences. When using 'AsyncSeq.ofObservable', values that are
// emitted when the asynchronous sequence is blocked are discarded. When you 
// click on the 'Async Operation' button, the following workflow starts
// processing and drops all clicks until the body of the for loop completes
let discarding =
  async {
    for click in btn.Click |> AsyncSeq.ofObservable do
      wprint "Sleeping (and discarding clicks)...\r\n"
      do! Async.Sleep(1000)
      wprint "Done (listening again)\r\n" }

let ctsd = new CancellationTokenSource()
Async.Start(discarding, ctsd.Token)
ctsd.Cancel()


// When using 'AsyncSeq.ofObservableBuffered', the values emitted by the 
// observable while the asynchronous sequence is blocked are stored in a 
// buffer (and will be returned as next elements). 
let buffering =
  async {
    for click in btn.Click |> AsyncSeq.ofObservableBuffered do
      wprint "Sleeping (and buffering clicks)...\r\n"
      do! Async.Sleep(1000)
      wprint "Done (ready for next value)\r\n" }

let ctsb = new CancellationTokenSource()
Async.Start(buffering, ctsb.Token)
ctsb.Cancel()