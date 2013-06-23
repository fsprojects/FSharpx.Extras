#r @"C:\Users\john\Documents\GitHub\fsharpx\src\FSharpx.TypeProviders.Excel\bin\Debug\FSharpx.TypeProviders.Excel.dll"

open FSharpx

type MultipleRegions = ExcelFile<"MultipleRegions.xlsx", "A1:C5,E3:G5", true>

let data = new MultipleRegions()
for row in data.Data do
    printfn "%s, %s, %s, %s, %s, %s" (row.First) (row.Second) (row.Third) (row.Fourth) (row.Fifth) (row.Sixth)