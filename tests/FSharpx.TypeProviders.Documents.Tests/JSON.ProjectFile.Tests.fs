module FSharpx.TypeProviders.Tests.JSON.ProjectFile.Tests

open NUnit.Framework
open FSharpx
open FsUnit

type Project = JsonZipper<"projects.json">

[<Test>]
let ``Can access the background title``() =
    let doc = new Project()
    let background = doc.Ordercontainer.Backgrounds.Background
    let title = background.Title
    title.Text.GetValue() |> should equal "purple stars"

[<Test>]
let ``Can access the project title``() =
    let doc = new Project()
    let project = doc.Ordercontainer.Project
    let title = project.Title
    title.Text.GetValue() |> should equal "Avery"