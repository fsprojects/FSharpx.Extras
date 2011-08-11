// ----------------------------------------------------------------------------
// F# async extensions (BatchProcessing.fsx)
// (c) Tomas Petricek, 2011, Available under Apache 2.0 license.
// ----------------------------------------------------------------------------

// This example demonstrates how to use 'BatchProcessingAgent'
// The agent groups received messages in groups with a maximal
// size and emits them with a maximal timeout.

#r "..\\bin\\FSharp.AsyncExtensions.dll"
open FSharp.Control

open System.Drawing
open System.Windows.Forms

// Create simple winforms user interface with label
let frm = new Form()
let lbl = new Label(Font = new Font("Calibri", 20.0f), Dock = DockStyle.Fill)
lbl.TextAlign <- ContentAlignment.MiddleCenter
frm.Controls.Add(lbl)
frm.Show()

// Handle key press events but update the GUI after 5 keys
// have been pressed or after 5 seconds (whichever happens first)
let ag = new BatchProcessingAgent<_>(5, 5000)
frm.KeyPress.Add(fun e -> ag.Enqueue(e.KeyChar))
ag.BatchProduced
  |> Event.map (fun chars -> new System.String(chars))
  |> Event.scan (+) ""
  |> Event.add (fun str -> lbl.Text <- str)

