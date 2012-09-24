module FSharpx.TypeProviders.Tests.JSON.ReaderTests

open NUnit.Framework
open FSharpx
open FsUnit

type InlinedJSON = StructuredJSON<Schema="""{ "firstName": "Max" "lastName": "Mustermann" "age": 26 "isCool": true }""">

[<Test>]
let ``Can parse inlined properties``() = 
    let inlined = InlinedJSON().Root
    inlined.FirstName
    |> should equal "Max"

    inlined.LastName
    |> should equal "Mustermann"

    inlined.Age
    |> should equal 26

    inlined.IsCool
    |> should equal true

[<Test>]
let ``Can parse inlined properties but read from file``() = 
    let inlined = InlinedJSON(filename="Simple.json").Root
    inlined.FirstName
    |> should equal "John"

    inlined.LastName
    |> should equal "Doe"

    inlined.Age
    |> should equal 25

    inlined.IsCool
    |> should equal true


type SimpleJSON = StructuredJSON<"Simple.json">

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

type NestedJSON = StructuredJSON<"Nested.json">

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

type DoubleNestedJSON = StructuredJSON<"DoubleNested.json">

let doubleNested = DoubleNestedJSON().Root

[<Test>]
let ``Can parse double nested properties``() = 
    doubleNested.Main.Title
    |> should equal "example"

    doubleNested.Main.Nested.NestedTitle
    |> should equal "sub"

type SimpleArrayJSON = StructuredJSON<"SimpleArray.json">

let simpleArray = SimpleArrayJSON().Root

[<Test>]
let ``Can parse simple arrays``() = 
    let items = simpleArray.GetItems() |> Seq.toList
    items.[0].Id
    |> should equal "Open"

    items.[1].Id
    |> should equal "Pause"

type OptionalValuesInJSON = StructuredJSON<"OptionValues.json">

let optionalValuesInJSON = OptionalValuesInJSON().Root

[<Test>]
let ``Can parse optional values in arrays``() = 
    let authors = optionalValuesInJSON.GetAuthors() |> Seq.toList
    authors.[0].Name
    |> should equal "Steffen"

    authors.[0].Age
    |> should equal (Some 29)

    authors.[1].Name
    |> should equal "Tomas"

    authors.[1].Age
    |> should equal None

[<Test>]
let ``Can compare typed JSON documents``() = 
    let simple1 = SimpleJSON().Root
    let simple2 = SimpleJSON().Root
    let nested = NestedJSON().Root

    Assert.AreEqual(simple1,simple2)
    Assert.AreNotEqual(nested,simple2)

type JsonArray = StructuredJSON<Schema="""["Adam","Eve","Bonnie","Clyde","Donald","Daisy","Han","Leia"]""">

[<Test>]
let ``Can parse simple array``() = 
    let inlined = JsonArray().Root    
    inlined.ToString()
      |> should equal """["Adam","Eve","Bonnie","Clyde","Donald","Daisy","Han","Leia"]"""

type MultipleJsonArray = StructuredJSON<Schema="""[["Adam","Eve"],["Bonnie","Clyde"],["Donald","Daisy"],["Han","Leia"]]""">

[<Test>]
let ``Can parse multidimensional arrays``() = 
    let inlined = MultipleJsonArray().Root
    inlined.ToString()
      |> should equal """[["Adam","Eve"],["Bonnie","Clyde"],["Donald","Daisy"],["Han","Leia"]]"""
