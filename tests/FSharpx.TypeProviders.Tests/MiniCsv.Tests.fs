module FSharp.TypeProviders.Tests.MiniCsvTests

open NUnit.Framework
open FSharpx.TypeProviders.JSON
open FsUnit

open FSharpx

open Data.UnitSystems.SI.UnitNames

type smallCsv = MinCsv<"SmallTest.csv">

[<Test>] 
let ``Can create type for small document``() = 
    let csv = smallCsv()
    let row1 = csv.Data |> Seq.head 

    row1.Distance |> should equal 50.<metre>
    let time = row1.Time // try doing "Go To Definition" on Time property
    time |> should equal 3.7<second>

[<Test>] 
let ``Can create type for document with same structure on different path``() = 
    let csv = smallCsv()
    let row1 = csv.Data |> Seq.head 

    row1.Distance |> should equal 55.<metre>