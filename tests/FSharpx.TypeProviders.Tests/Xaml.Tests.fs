module FSharp.TypeProviders.Tests.XamlTests

open NUnit.Framework
open FSharpx
open FsUnit

type T = XamlFile< @"StackPanel.xaml">
let window = T()

[<Test>] 
let ``Can access the grid``() =      
   window.MainGrid.Name |> should equal "MainGrid"

[<Test>] 
let ``Can access the stackpanel``() =      
   // TODO: this should be window.Maingrind.StackPanel1
   window.StackPanel1.Name |> should equal "StackPanel1"

[<Test>] 
let ``Can access the first button``() =      
   // TODO: this should be window.Maingrind.StackPanel1.Button1
   window.Button1.Name |> should equal "Button1"

[<Test>] 
let ``The window should have the right type``() =
   window.GetType() |> should equal typeof<System.Windows.Window>

[<Test>] 
let ``The grid should have the right type``() =
   window.MainGrid.GetType() |> should equal typeof<System.Windows.Controls.Grid>

[<Test>] 
let ``The button should have the right type``() =
   window.Button2.GetType() |> should equal typeof<System.Windows.Controls.Button>
