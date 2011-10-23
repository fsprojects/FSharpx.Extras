module FSharp.TypeProviders.Tests.XamlTests

open NUnit.Framework
open FSharpx
open FsUnit

type T = XamlFile< @"StackPanel.xaml">

[<Test>][<RequiresSTA>]
let ``Can access the grid``() =      
   let window = T()
   window.MainGrid.Control.Name |> should equal "MainGrid"

[<Test>][<RequiresSTA>]
let ``Can access the stackpanel``() =      
   let window = T()
   window.MainGrid.StackPanel1.Control.Name |> should equal "StackPanel1"

[<Test>][<RequiresSTA>]
let ``Can access the first button``() =      
   let window = T()
   window.MainGrid.StackPanel1.Button1.Control.Name |> should equal "Button1"

[<Test>][<RequiresSTA>]
let ``The window should have the right type``() =
   let window = T()
   window.Control.GetType() |> should equal typeof<System.Windows.Window>

[<Test>][<RequiresSTA>]
let ``The grid should have the right type``() =
   let window = T()
   window.MainGrid.Control.GetType() |> should equal typeof<System.Windows.Controls.Grid>

[<Test>][<RequiresSTA>]
let ``The button should have the right type``() =
   let window = T()
   window.MainGrid.StackPanel1.Button2.Control.GetType() |> should equal typeof<System.Windows.Controls.Button>
