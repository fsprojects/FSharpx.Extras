module FSharpx.TypeProviders.Tests.MiniCsvTests

open NUnit.Framework
open FsUnit
open FSharpx
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

type SmallCsv = MinCsv<"SmallTest.csv">

[<Test>] 
let ``Can create type for small document``() =
    let row1 = SmallCsv().Data |> Seq.head 

    row1.Distance |> should equal 50.<metre>
    let time = row1.Time // try doing "Go To Definition" on Time property
    time |> should equal 3.7<second>