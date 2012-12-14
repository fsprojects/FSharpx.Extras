module FSharpx.TypeProviders.Tests.XamlTests

open NUnit.Framework
open FSharpx
open FsUnit

type StackPanel = XAML<"StackPanel.xaml">
 
[<Test>][<RequiresSTA>]
let ``Can access the grid``() =      
   StackPanel().MainGrid.Name |> should equal "MainGrid"

[<Test>][<RequiresSTA>]
let ``Can access the stackpanel``() =      
   StackPanel().StackPanel1.Name |> should equal "StackPanel1"

[<Test>][<RequiresSTA>]
let ``Can access the stackpanel from cache``() =      
   let window = StackPanel()
   window.StackPanel1.Name |> should equal "StackPanel1"
   window.StackPanel1.Name |> should equal "StackPanel1" // this goes through the cache

[<Test>][<RequiresSTA>]
let ``Internal components have obj at design-time but correct at run-time``() =      
   let int = StackPanel().InternalComponent
   int.GetType() |> should equal typedefof<any.MyButton>

//[<Test>][<RequiresSTA>]
//let ``External components have proper types``() =      
//   let ext = StackPanel().ExternalComponent
//   let clickmode = ext.ClickMode // Check compile-time type by accessing something
//   ext.GetType() |> should equal typedefof<TypeProviders.Tests.Xaml.MyExternalButton> // Check run-time type

[<Test>][<RequiresSTA>]
let ``Can access the first button``() =      
   StackPanel().Button1.Name |> should equal "Button1"

[<Test>][<RequiresSTA>]
let ``The window should have the right type``() =
   StackPanel().Root.GetType() |> should equal typeof<System.Windows.Window>

[<Test>][<RequiresSTA>]
let ``The grid should have the right type``() =
   StackPanel().MainGrid.GetType() |> should equal typeof<System.Windows.Controls.Grid>

[<Test>][<RequiresSTA>]
let ``The button should have the right type``() =
   StackPanel().Button2.GetType() |> should equal typeof<System.Windows.Controls.Button>

type NamedRoot = XAML<"NamedRoot.xaml">

[<Test>][<RequiresSTA>]
let ``If the root has a name then just take this``() =
   let window = NamedRoot()
   window.MainWindow.Name |> should equal "MainWindow"

type Inlined = 
    XAML<Schema =
        """<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
               Title="MainWindow" Height="350" Width="525" Name="MainWindow" >
                <Button Name="Button1">First Button</Button>
           </Window>""">

[<Test>][<RequiresSTA>]
let ``It should parse inlined xaml``() =
   let window = Inlined()
   window.MainWindow.Name |> should equal "MainWindow"

type UnnamedControls = 
    XAML<Schema =
        """<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
                    Title="MainWindow" Height="350" Width="525">
                <Grid Name="MainGrid">
                    <StackPanel>
                        <Button>First Button</Button>
                        <Button Name="Button1">Second Button</Button>
                    </StackPanel>
                </Grid>
            </Window>""">

[<Test>][<RequiresSTA>]
let ``It should allow to skip unnamed controls in xaml``() =
   let window = UnnamedControls()
   window.Button1.GetType() |> should equal typeof<System.Windows.Controls.Button>