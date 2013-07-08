module FSharpx.TypeProviders.Tests.ExcelTests

open NUnit.Framework
open FSharpx
open FsUnit

open System
open System.IO

type BookTest = ExcelFile<"BookTest.xls", "Sheet1", true>
type HeaderTest = ExcelFile<"BookTestWithHeader.xls", "A2", true>
type MultipleRegions = ExcelFile<"MultipleRegions.xlsx", "A1:C5,E3:G5", true>

[<Test>]
let ``Can access first row in typed excel data``() = 
    let file = BookTest()
    let row = file.Data |> Seq.head
    row.SEC |> should equal "ASI"
    row.BROKER |> should equal "TFS Derivatives HK"

[<Test>]
let ``Can pick an arbitrary header row``() =
    let file = HeaderTest()
    let row = file.Data |> Seq.head
    row.SEC |> should equal "ASI"
    row.BROKER |> should equal "TFS Derivatives HK"

[<Test>]
let ``Can load data from spreadsheet``() =
    let file = Path.Combine(Environment.CurrentDirectory, "BookTestDifferentData.xls")

    printfn "%s" file   

    let otherBook = BookTest(file)
    let row = otherBook.Data |> Seq.head

    row.SEC |> should equal "TASI"
    row.STYLE |> should equal "B"
    row.``STRIKE 1`` |> should equal "3"
    row.``STRIKE 2`` |> should equal "4"
    row.``STRIKE 3`` |> should equal "5"
    row.VOL |> should equal "322"

[<Test>]
let ``Can load from multiple ranges``() =
    let file = MultipleRegions()
    let rows = file.Data |> Seq.toArray

    rows.[0].First |> should equal "A1"
    rows.[0].Second |> should equal "A2"
    rows.[0].Third |> should equal "A3"
    rows.[0].Fourth |> should equal "B1"
    rows.[0].Fifth |> should equal "B2"
    rows.[0].Sixth |> should equal "B3"

    rows.[1].First |> should equal "A4"
    rows.[1].Second |> should equal "A5"
    rows.[1].Third |> should equal "A6"
    rows.[1].Fourth |> should equal "B4"
    rows.[1].Fifth |> should equal "B5"
    rows.[1].Sixth |> should equal "B6"

    rows.[2].First |> should equal "A7"
    rows.[2].Second |> should equal "A8"
    rows.[2].Third |> should equal "A9"
    rows.[2].Fourth |> should equal null
    rows.[2].Fifth |> should equal null
    rows.[2].Sixth |> should equal null

    rows.[3].First |> should equal "A10"
    rows.[3].Second |> should equal "A11"
    rows.[3].Third |> should equal "A12"
    rows.[3].Fourth |> should equal null
    rows.[3].Fifth |> should equal null
    rows.[3].Sixth |> should equal null