#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif

open System
open System.IO
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.ExcelProvider

let (++) a b = Path.Combine(a, b)
let resolutionFolder = __SOURCE_DIRECTORY__ ++ ".." ++ ".." ++ "tests" ++ "FSharpx.TypeProviders.Excel.Tests"

generate (fun _ cfg -> typExcel cfg) resolutionFolder [| box "BookTest.xls"; box "Sheet1"; box true; box 1 |] 
|> prettyPrint 
|> Console.WriteLine