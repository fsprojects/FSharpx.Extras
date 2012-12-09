#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif

open System
open System.IO
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.XamlProvider

let (++) a b = Path.Combine(a, b)
let resolutionFolder = __SOURCE_DIRECTORY__ ++ ".." ++ ".." ++ "tests" ++ "FSharpx.TypeProviders.Xaml.Tests"

generate xamlType resolutionFolder [| "StackPanel.xaml"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate xamlType resolutionFolder [| "NamedRoot.xaml"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

let resolutionFolder2 = __SOURCE_DIRECTORY__ ++ ".." ++ ".." ++ "samples" ++ "WPF" ++ "WPFSample"

generate xamlType resolutionFolder2 [| "Window.xaml"; missingValue |] 
|> prettyPrint 
|> Console.WriteLine

generate xamlType "" [| missingValue
                        """<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                                   xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
                                  Title="MainWindow" Height="350" Width="525" Name="MainWindow" >
                                   <Button Name="Button1">First Button</Button>
                              </Window>""" |] 
|> prettyPrint 
|> Console.WriteLine
