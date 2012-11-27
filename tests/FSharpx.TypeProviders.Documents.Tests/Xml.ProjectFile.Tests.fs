module FSharpx.TypeProviders.Tests.Xml.ProjectFile.Tests

open NUnit.Framework
open FSharpx
open FsUnit

type Project = StructuredXml<"projects.xml">

[<Test>]
let ``Can access the background title``() =
    let doc = new Project()
    let background = (doc.Root.GetBackgrounds() |> Seq.head).GetBackgrounds() |> Seq.head
    let title = background.GetTitles() |> Seq.head
    title.Element.Value |> should equal "purple stars"

[<Test>]
let ``Can access the project title``() =
    let doc = new Project()
    let project = doc.Root.GetProjects() |> Seq.head
    let title = project.GetTitles() |> Seq.head
    title.Element.Value |> should equal "Avery"