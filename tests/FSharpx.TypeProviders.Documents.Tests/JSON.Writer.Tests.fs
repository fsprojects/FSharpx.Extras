module FSharpx.TypeProviders.Tests.JSON.WriterTests

open NUnit.Framework
open FSharpx
open FsUnit

type InlinedJSON = JsonZipper<Schema="""{ "firstName": "Max","lastName": "Mustermann", "age": 26, "isCool": true, "size":42.42 }""">

[<Test>]
let ``Can set properties in inlined JSON``() = 
    let person = new InlinedJSON()

    let updated =
        person
            .FirstName.Update("John")
            .LastName.Update("Doe")
            .Age.Update(30)
            .IsCool.Update(false)
            .Size.Update(decimal 43.43)

    updated.FirstName.GetValue() |> should equal "John"
    updated.LastName.GetValue() |> should equal "Doe"
    updated.Age.GetValue() |> should equal 30
    updated.IsCool.GetValue() |> should equal false
    updated.Size.GetValue() |> should equal 43.43

    person.FirstName.GetValue() |> should equal "Max"
    person.LastName.GetValue() |> should equal "Mustermann"
    person.Age.GetValue() |> should equal 26
    person.IsCool.GetValue() |> should equal true
    person.Size.GetValue() |> should equal 42.42

    updated.ToString() |> should equal """{"firstName":"John","lastName":"Doe","age":30,"isCool":false,"size":43.43}"""


type AuthorsJSON = JsonZipper<Schema="""{ "authors": [{ "name": "Steffen" }, { "name": "Tomas", "age": 29, "isCool": true, "size":42.42 }]}""">

//[<Test>]
//let ``Can set optional properties in inlined JSON``() = 
//    let inlined = new AuthorsJSON()
//
//    let author = inlined.GetAuthors() |> Seq.head
//
//    author.Age <- None
//    author.Age |> should equal None
//
//    author.Age <- Some 42
//    author.Age |> should equal (Some 42)
//
//    author.IsCool <- None
//    author.IsCool |> should equal None
//
//    author.IsCool <- Some true
//    author.IsCool |> should equal (Some true)
//
//    author.Size <- None
//    author.Size |> should equal None
//
//    author.Size <- Some (decimal 42.45)
//    author.Size |> should equal (Some (decimal 42.45))

//[<Test>]
//let ``Can add author in inlined JSON``() = 
//    let inlined = new AuthorsJSON()
//
//    let author = inlined.Root.NewAuthor()
//    author.Name <- "John"
//    
//    inlined.Root.AddAuthor author
//
//    let authors = inlined.Root.GetAuthors() |> Seq.toList
//    authors.Length |> should equal 3
//
//    authors.[0].Name |> should equal "Steffen"
//    authors.[1].Name |> should equal "Tomas"
//    authors.[2].Name |> should equal "John"

[<Test>]
let ``Can serialize the json``() =
    let inlined = new AuthorsJSON()
    let json = inlined.ToString()
    json |> should equal """{"authors":[{"name":"Steffen"},{"name":"Tomas","age":29,"isCool":true,"size":42.42}]}"""

open System.Xml.Linq

[<Test>]
let ``Can convert the json to xml``() =
    let inlined = new AuthorsJSON()
    let xml = inlined.ToXml() |> Seq.head 
    let expectedXml = XDocument.Parse("<authors><item name=\"Steffen\" /><item name=\"Tomas\" age=\"29\" isCool=\"true\" size=\"42.42\" /></authors>")
    xml.ToString() |> should equal (expectedXml.ToString())
