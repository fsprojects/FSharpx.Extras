module FSharpx.TypeProviders.Tests.ExcelTests

open NUnit.Framework
open FSharpx
open FsUnit


type BookTest = FSharpx.ExcelFile<"BookTest.xls", "Sheet1", true>

let file = BookTest()
let row1 = file.Data |> Seq.head 

[<Test>] 
let ``Can access first row in typed excel data``() =    
    row1.SEC |> should equal "ASI"
    row1.BROKER |> should equal "TFS Derivatives HK"