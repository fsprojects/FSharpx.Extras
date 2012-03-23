open System
open System.Windows
open System.Windows.Controls
open FSharpx

type MainWindow = XamlFile<"Window.xaml">

let loadWindow() =
   let window = MainWindow()
   window.Button1.Click.Add(
        fun _ -> MessageBox.Show("Hello world!") |> ignore)
   window.Control

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore