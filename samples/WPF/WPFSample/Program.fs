open System
open System.Windows
open System.Windows.Controls

let loadWindow() =    
    let resourceLocator = new Uri("/WPFSample;component/Window.xaml", UriKind.Relative)
    let window = Application.LoadComponent(resourceLocator) :?> Window
    (window.FindName("Button1") :?> Button).Click.Add(
        fun _ -> MessageBox.Show("Hello world!") |> ignore)
    window

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore