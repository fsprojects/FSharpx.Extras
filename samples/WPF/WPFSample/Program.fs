open System
open System.Windows
open System.Windows.Controls
open FSharpx

type MainWindow = XAML<"Window.xaml">

let loadWindow() =
    let window = MainWindow()
    window.Button1.Click.Add(fun _ -> 
        MessageBox.Show("Hello world!") 
        |> ignore)
    window.MainWindow

[<STAThread>]
(new Application()).Run(loadWindow()) 
|> ignore