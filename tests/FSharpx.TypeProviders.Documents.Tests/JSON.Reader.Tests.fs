module FSharpx.TypeProviders.Tests.JSON.ReaderTests

open NUnit.Framework
open FSharpx
open FsUnit

type InlinedJSON = JsonZipper<Schema="""{ "firstName": "Max","lastName": "Mustermann","age": 26,"isCool": true }""">

[<Test>]
let ``Can parse inlined properties``() = 
    let inlined = InlinedJSON()
    inlined.FirstName.GetValue()
    |> should equal "Max"

    inlined.LastName.GetValue()
    |> should equal "Mustermann"

    inlined.Age.GetValue()
    |> should equal 26

    inlined.IsCool.GetValue()
    |> should equal true

[<Test>]
let ``Can parse inlined properties but read from file``() = 
    let inlined = InlinedJSON(filename="Simple.json")
    inlined.FirstName.GetValue()
    |> should equal "John"

    inlined.LastName.GetValue()
    |> should equal "Doe"

    inlined.Age.GetValue()
    |> should equal 25

    inlined.IsCool.GetValue()
    |> should equal true


type SimpleJSON = JsonZipper<"Simple.json">

let simple = SimpleJSON()

[<Test>]
let ``Can parse properties``() = 
    simple.FirstName.GetValue()
    |> should equal "John"

    simple.LastName.GetValue()
    |> should equal "Doe"

    simple.Age.GetValue()
    |> should equal 25

    simple.IsCool.GetValue()
    |> should equal true

type NestedJSON = JsonZipper<"Nested.json">

let nested = NestedJSON()

[<Test>]
let ``Can parse nested properties``() = 
    nested.Main.FirstName.GetValue()
    |> should equal "John"

    nested.Main.LastName.GetValue()
    |> should equal "Doe"

    nested.Main.Age.GetValue()
    |> should equal 25

    nested.Main.IsCool.GetValue()
    |> should equal true

type DoubleNestedJSON = JsonZipper<"DoubleNested.json">

let doubleNested = DoubleNestedJSON()

[<Test>]
let ``Can parse double nested properties``() = 
    doubleNested.Main.Title.GetValue()
    |> should equal "example"

    doubleNested.Main.Nested.NestedTitle.GetValue()
    |> should equal "sub"

type SimpleArrayJSON = JsonZipper<"SimpleArray.json">

let simpleArray = SimpleArrayJSON()

[<Test>]
let ``Can parse simple arrays``() = 
    let items = simpleArray.Items
    items.GetElement(0).Id.GetValue()
    |> should equal "Open"

    items.GetElement(1).Id.GetValue()
    |> should equal "Pause"

type OptionalValuesInJSON = JsonZipper<"OptionValues.json">

let optionalValuesInJSON = OptionalValuesInJSON()

[<Test>]
let ``Can parse optional values in arrays``() = 
    let authors = optionalValuesInJSON.Authors
    authors.GetElement(0).Name.GetValue()
    |> should equal "Steffen"

    authors.GetElement(0).Age.GetValue()
    |> should equal (Some 29)

    authors.GetElement(1).Name.GetValue()
    |> should equal "Tomas"

    authors.GetElement(1).Age.GetValue()
    |> should equal None

[<Test>]
let ``Can compare typed JSON documents``() = 
    let simple1 = SimpleJSON()
    let simple2 = SimpleJSON()
    let nested = NestedJSON()

    Assert.AreEqual(simple1,simple2)
    Assert.AreNotEqual(nested,simple2)

type JsonArray = JsonZipper<Schema="""["Adam","Eve","Bonnie","Clyde","Donald","Daisy","Han","Leia"]""">

[<Test>]
let ``Can parse simple array``() = 
    let inlined = JsonArray() 
    inlined.ToString()
      |> should equal """["Adam","Eve","Bonnie","Clyde","Donald","Daisy","Han","Leia"]"""

type MultipleJsonArray = JsonZipper<Schema="""[["Adam","Eve"],["Bonnie","Clyde"],["Donald","Daisy"],["Han","Leia"]]""">

[<Test>]
let ``Can parse multidimensional arrays``() = 
    let inlined = MultipleJsonArray()
    inlined.ToString()
      |> should equal """[["Adam","Eve"],["Bonnie","Clyde"],["Donald","Daisy"],["Han","Leia"]]"""
