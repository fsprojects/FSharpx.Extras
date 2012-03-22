module FSharp.TypeProviders.Tests.Xml.Reader.Tests

open NUnit.Framework
open FSharpx
open FsUnit

let inlined = new StructuredXml<Schema="""<authors><author name="Ludwig" surname="Wittgenstein" /></authors>""">()

[<Test>]
let ``Can get author name in inlined xml``() = 
    let author = inlined.Root.GetAuthorElements() |> Seq.head
    author.Name |> should equal "Ludwig"
    author.Surname |> should equal "Wittgenstein"

let philosophy = new StructuredXml<"Philosophy.xml">()
let authors = philosophy.Root.GetAuthorElements() |> Seq.toList

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
    let books = authors.[0].GetBookElements() |> Seq.toList
    books.[0].Title |> should equal "Tractatus Logico-Philosophicus"
    books.[1].Title |> should equal "Philosophical Investigations"