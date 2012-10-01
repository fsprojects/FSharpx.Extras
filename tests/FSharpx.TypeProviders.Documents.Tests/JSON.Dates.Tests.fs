module FSharpx.TypeProviders.Tests.JSON.DatesTests

open NUnit.Framework
open FSharpx
open FsUnit
open System

type DateJSON = StructuredJSON<"Dates.json">

[<Test>]
let ``Can parse microsoft format dates``() = 
    let dates = new DateJSON()
    dates.Root.Birthdate |> should equal (new DateTime(1997, 7, 16, 19, 20, 30, 450)) // 1997-07-16T19:20:30.45+01:00

[<Test>]
let ``Can parse ISO 8601 dates``() =
    let dates = new DateJSON()
    dates.Root.Anniversary |> should equal (new DateTime(1997, 7, 16, 19, 20, 30, 450)) 
