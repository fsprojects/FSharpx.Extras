#I __SOURCE_DIRECTORY__
#r @"bin\Debug\FSharpx.TypeProviders.Excel.dll"

open FSharpx

type MultipleRegions = ExcelFile<"MultipleRegions.xlsx", "A1:C5,E3:G5", true>

let data = new MultipleRegions()
for row in data.Data do
    printfn "%A, %A, %A, %A, %A, %A" (row.First) (row.Second) (row.Third) (row.Fourth) (row.Fifth) (row.Sixth)
	
type DataTypesTest = ExcelFile<"DataTypes.xlsx">
let file = new DataTypesTest()
let row = file.Data |> Seq.head

printfn "%A" (row.String)
printfn "%A" (row.Int)
printfn "%A" (row.Float)
printfn "%A" (row.Boolean)
printfn "%A" (row.DateTime)
