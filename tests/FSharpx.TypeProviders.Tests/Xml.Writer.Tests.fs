module FSharp.TypeProviders.Tests.Xml.Writer.Tests

open NUnit.Framework
open FSharpx
open FsUnit
open System.Xml.Linq

let inlined = new StructuredXml<Schema="""<authors><author name="Ludwig" surname="Wittgenstein" age="29" isPhilosopher="True" size="30.3" /></authors>""">()
let author = inlined.Root.GetAuthorElements() |> Seq.head

[<Test>]
let ``Can set properties in inlined xml``() = 
    author.Name <- "John"
    author.Name |> should equal "John"

    author.Age <- 30
    author.Age |> should equal 30

    author.IsPhilosopher <- false
    author.IsPhilosopher |> should equal false

    author.Size <- 42.42
    author.Size |> should equal 42.42

[<Test>]
let ``Can export modified xml``() = 
    author.Name <- "John"
    author.Age <- 31
    author.IsPhilosopher <- false
    author.Size <- 22.2

    inlined.Document.ToString(SaveOptions.DisableFormatting)
      .Replace("22,2","22.2")  // TODO: Use  InvariantCulture 
    |> should equal """<authors><author name="John" surname="Wittgenstein" age="31" isPhilosopher="False" size="22.2" /></authors>"""