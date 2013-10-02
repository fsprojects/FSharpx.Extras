module FSharpx.TypeProviders.Tests.AppSettingsTests

open NUnit.Framework
open FSharpx
open FsUnit

type Settings = AppSettings<"App.config">

[<Test>] 
let ``Can return a string from the config file``() =   
    Settings.Test2.GetType() |> should equal typeof<string>   
    Settings.Test2 |> should equal "Some Test Value 5"

[<Test>] 
let ``Can return an integer from the config file``() =
    Settings.TestInt.GetType() |> should equal typeof<int>
    Settings.TestInt |> should equal 102

[<Test>] 
let ``Can return a double from the config file``() =
    Settings.TestDouble.GetType() |> should equal typeof<float>
    Settings.TestDouble |> should equal 10.01

[<Test>] 
let ``Can return a boolean from the config file``() =
    Settings.TestBool.GetType() |> should equal typeof<bool>
    Settings.TestBool |> should equal true