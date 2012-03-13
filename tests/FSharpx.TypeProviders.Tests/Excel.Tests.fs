module FSharp.TypeProviders.Tests.ExcelTests

open NUnit.Framework
open FSharpx
open FsUnit


type BookTest = FSharpx.ExcelFile<"BookTest.xls", "Sheet1", true>


[<Test>] 
let ``Can call typed IsMatch function``() =    
    let file =BookTest()
    let row1 = file.Data |> Seq.head 

    row1.SEC |> should equal "ASI" 