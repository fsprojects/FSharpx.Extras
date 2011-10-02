module FSharp.TypeProviders.Tests.AppSettingsTests

open NUnit.Framework
open FSharpx
open FsUnit

type T = AppSettingsTyped< @"Test.App.config">

[<Test>] 
let ``Can return a string from the config file``() =      
   T.test2 |> should equal "Some Test Value 5"

[<Test>] 
let ``Can return an integer from the config file``() =      
    T.TestInt |> should equal 102

[<Test>] 
let ``Can return a double from the config file``() =
    T.TestDouble |> should equal 10.01

[<Test>] 
let ``Can return a boolean from the config file``() =
    T.TestBool |> should equal true
