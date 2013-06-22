module FSharpx.TypeProviders.Tests.ExcelTests
#r @"C:\Users\johna\Documents\GitHub\fsharpx\src\FSharpx.TypeProviders.Excel\bin\Debug\FSharpx.TypeProviders.Excel.dll"

open FSharpx

open System
open System.IO


type MultipleRegions = ExcelFile<"MultipleRegions.xlsx", "A1:C5,E3:G5", true>