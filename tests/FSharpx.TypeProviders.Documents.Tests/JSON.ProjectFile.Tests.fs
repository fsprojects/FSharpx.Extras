module FSharpx.TypeProviders.Tests.JSON.ProjectFile.Tests

open NUnit.Framework
open FSharpx
open FsUnit

type Project = StructuredJSON<"projects.json">

[<Test>]
let ``Can access the background title``() =
    let doc = new Project()
    let background = doc.Root.Ordercontainer.Backgrounds.Background
    let title = background.Title
    title.Text |> should equal "purple stars"

[<Test>]
let ``Can access the project title``() =
    let doc = new Project()
    let project = doc.Root.Ordercontainer.Project
    let title = project.Title
    title.Text |> should equal "Avery"