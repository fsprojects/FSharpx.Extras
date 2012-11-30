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
    dates.Root.Anniversary.ToUniversalTime() |> should equal (new DateTime(1997, 7, 16, 18, 20, 30, 450)) 

[<Test>]
let ``Can parse UTC dates``() =
    let dates = new DateJSON()
    dates.Root.UtcTime.ToUniversalTime() |> should equal (new DateTime(1997, 7, 16, 19, 50, 30, 0)) 

[<Test>]
[<SetCulture("zh-CN")>]
let ``Can parse ISO 8601 dates in the correct culture``() =
    let dates = new DateJSON()
    dates.Root.NoTimeZone |> should equal (new DateTime(1997, 7, 16, 19, 20, 30, 00, System.DateTimeKind.Local)) 

type singleDate = StructuredJSON<Schema="""{ "anniversary" : "1990-01-01" }""">
    
[<Test>]
let ``Set a Date``() =
    let dates = new singleDate()
    dates.Root.Anniversary <- new DateTime(2012, 1, 29, 12, 30, 00, DateTimeKind.Utc)
    dates.ToString() |>  should equal """{"anniversary":"2012-01-29T12:30:00.0000000Z"}"""

[<Test>]
let ``Round Trip a Date``() =
    let dates = new singleDate()
    dates.Root.Anniversary <- new DateTime(2012, 1, 29, 12, 30, 00, DateTimeKind.Utc)
    dates.ToString() |>  should equal """{"anniversary":"2012-01-29T12:30:00.0000000Z"}"""
    let dates2 = singleDate(documentContent=(dates.ToString()))
    dates2.Root.Anniversary |> should equal (new DateTime(2012, 1, 29, 12, 30, 00, DateTimeKind.Utc))