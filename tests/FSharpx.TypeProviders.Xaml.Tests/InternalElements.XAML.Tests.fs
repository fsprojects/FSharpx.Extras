namespace FSharpx.TypeProviders.InternalElements

type MyButton() =
   inherit System.Windows.Controls.Button()

open NUnit.Framework
open FSharpx
open FsUnit

type Inlined = 
    XAML<Schema =
        """<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
                    xmlns:internalE="clr-namespace:FSharpx.TypeProviders.InternalElements">
                <Grid>
                    <internalE:MyButton x:Name="CustomComponent">This explodes</internalE:MyButton>
                </Grid>
            </Window>""">

module Tests =
    [<Test>][<RequiresSTA>]
    let ``It should parse xaml with internal elements``() =
       let window = Inlined()
       window.CustomComponent |> should equal "MainWindow"