module FSharp.TypeProviders.Tests.JSONTests

open NUnit.Framework
open FSharpx
open FsUnit

type SimpleJSON = StructuredJSON< "SimpleJSON.txt">

let simple = SimpleJSON().Root

[<Test>]
let ``Can parse properties``() = 
    simple.FirstName
    |> should equal "John"

    simple.LastName
    |> should equal "Doe"

    simple.Age
    |> should equal 25

    simple.IsCool
    |> should equal true


type NestedJSON = StructuredJSON< "NestedJSON.txt">

let nested = NestedJSON().Root

[<Test>]
let ``Can parse nested properties``() = 
    nested.Main.FirstName
    |> should equal "John"

    nested.Main.LastName
    |> should equal "Doe"

    nested.Main.Age
    |> should equal 25

    nested.Main.IsCool
    |> should equal true

type DoubleNestedJSON = StructuredJSON< "DoubleNestedJSON.txt">

let doubleNested = DoubleNestedJSON().Root

[<Test>]
let ``Can parse double nested properties``() = 
    doubleNested.Main.Title
    |> should equal "example"

    doubleNested.Main.Nested.NestedTitle
    |> should equal "sub"

type SimpleArrayJSON = StructuredJSON< "SimpleArrayJSON.txt">

let simpleArray = SimpleArrayJSON().Root

[<Test>]
let ``Can parse simple arrays``() = 
    let items = simpleArray.GetItemsElements() |> Seq.toList
    items.[0].Id
    |> should equal "Open"

    items.[1].Id
    |> should equal "Pause"