module FSharpx.TypeProviders.Tests.Xml.Reader.Tests

open NUnit.Framework
open FSharpx
open FsUnit

let inlined = new StructuredXml<Schema="""<authors><author name="Ludwig" surname="Wittgenstein" /></authors>""">()

[<Test>]
let ``Can get author name in inlined xml``() = 
    let author = inlined.Root.GetAuthors() |> Seq.head
    author.Name |> should equal "Ludwig"
    author.Surname |> should equal "Wittgenstein"

let philosophy = new StructuredXml<"Philosophy.xml">()
let authors = philosophy.Root.GetAuthors() |> Seq.toList

[<Test>]
let ``Can get author names in philosophy.xml``() = 
    authors.[0].Name |> should equal "Ludwig"
    authors.[1].Name |> should equal "Rene"

[<Test>]
let ``Can get author surnames in philosophy.xml``() = 
    authors.[0].Surname |> should equal "Wittgenstein"
    authors.[1].Surname |> should equal "Descartes"

[<Test>]
let ``Can get the optional author birthday in philosophy.xml``() = 
    authors.[0].Birth |> should equal None
    authors.[1].Birth |> should equal (Some 1596)

[<Test>]
let ``Can get Descartes books in philosophy.xml``() = 
    let books = authors.[0].GetBooks() |> Seq.toList
    books.[0].Title |> should equal "Tractatus Logico-Philosophicus"
    books.[1].Title |> should equal "Philosophical Investigations"

    
let topicDocument = new StructuredXml<Schema="""<topics><topic><title>My Topic Title</title></topic><topic><title>Another Topic Title</title></topic></topics>""">()

[<Test>]
let ``Can get a title of the first topic``() = 
    let topics = topicDocument.Root.GetTopics() |> Seq.toList
    (topics.[0].GetTitles() |> Seq.head).Element.Value |> should equal "My Topic Title"
    (topics.[1].GetTitles() |> Seq.head).Element.Value |> should equal "Another Topic Title"