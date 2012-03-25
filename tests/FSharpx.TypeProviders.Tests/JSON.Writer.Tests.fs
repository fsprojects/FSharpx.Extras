module FSharp.TypeProviders.Tests.JSON.WriterTests

open NUnit.Framework
open FSharpx
open FsUnit

type InlinedJSON = StructuredJSON<Schema="""{ "firstName": "Max" "lastName": "Mustermann" "age": 26 "isCool": true, "size":42.42 }""">

[<Test>]
let ``Can set properties in inlined properties``() = 
    let inlined = new InlinedJSON()
    let person = inlined.Root

    person.FirstName <- "John"
    person.FirstName |> should equal "John"

    person.LastName <- "Doe"
    person.LastName |> should equal "Doe"

    person.Age <- 30
    person.Age |> should equal 30

    person.IsCool <- false
    person.IsCool |> should equal false

    person.Size <- 43.43
    person.Size |> should equal 43.43

type AuthorsJSON = StructuredJSON<Schema="""{ "authors": [{ "name": "Steffen" }, { "name": "Tomas", "age": 29, "isCool": true, "size":42.42 }]}""">

[<Test>]
let ``Can set optional properties in inlined JSON``() = 
    let inlined = new AuthorsJSON()

    let author = inlined.Root.GetAuthorsElements() |> Seq.head

    author.Age <- None
    author.Age |> should equal None

    author.Age <- Some 42
    author.Age |> should equal (Some 42)

    author.IsCool <- None
    author.IsCool |> should equal None

    author.IsCool <- Some true
    author.IsCool |> should equal (Some true)

    author.Size <- None
    author.Size |> should equal None

    author.Size <- Some 42.45
    author.Size |> should equal (Some 42.45)

[<Test>]
let ``Can add author in inlined JSON``() = 
    let inlined = new AuthorsJSON()

    let author = inlined.Root.NewAuthors()
    author.Name <- "John"
    
    inlined.Root.AddAuthors author

    let authors = inlined.Root.GetAuthorsElements() |> Seq.toList
    authors.Length |> should equal 3

    authors.[0].Name |> should equal "Steffen"
    authors.[1].Name |> should equal "Tomas"
    authors.[2].Name |> should equal "John"