module FSharpx.TypeProviders.Tests.ExcelTests

open NUnit.Framework
open FSharpx
open FsUnit

open System
open System.IO

type BookTest = ExcelFile<"BookTest.xls", "Sheet1", true>
type HeaderTest = ExcelFile<"BookTestWithHeader.xls", "A2", true>
type MultipleRegions = ExcelFile<"MultipleRegions.xlsx", "A1:C5,E3:G5", true>

let file = BookTest()
let row1 = file.Data |> Seq.head 

[<Test>]
let ``Can access first row in typed excel data``() = 
    row1.SEC |> should equal "ASI"
    row1.BROKER |> should equal "TFS Derivatives HK"

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
    let firstRow = file.Data |> Seq.head

    row.
