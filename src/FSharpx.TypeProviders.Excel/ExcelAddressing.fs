module internal FSharpx.TypeProviders.ExcelAddressing
    open System
    open System.Data
    open System.Text.RegularExpressions

    type Address = {
        Sheet: string;
        Row: int;
        Column: int}

    type Range =
        | Bounded of Address * Address
        | Unbounded of Address

    ///Parses an excel row column address of the form <COLUMN LETTERS><ROW NUMBER> into a zero based row, column address.
    ///For example the address A1 would be parsed as 0, 0
    let parseCellAddress cellAddress =

        let convertToBase radix digits =
            let digitValue i digit = float digit * Math.Pow(float radix, float i)

            digits
            |> List.rev
            |> List.mapi digitValue
            |> Seq.sum
            |> int
        
        let charToDigit char = ((int)(Char.ToUpper(char))) - 64

        let column = 
            cellAddress 
            |> Seq.filter Char.IsLetter 
            |> Seq.map charToDigit
            |> Seq.toList
            |> convertToBase 26

        let row =
            cellAddress
            |> Seq.filter Char.IsNumber
            |> Seq.map (string >> Int32.Parse)
            |> Seq.toList
            |> convertToBase 10

        (row - 1), (column - 1)

    let addressParser = new Regex("(?<sheet>[^!]*)!?(?<cell>\w+\d+)")

    ///Parses an excel address from a string
    ///Valid inputs look like:
    ///Sheet!A1
    ///B3
    let parseExcelAddress sheetContext address =
        let regexMatch = addressParser.Match(address)
        let sheetGroup = regexMatch.Groups.Item("sheet")
        let cellGroup = regexMatch.Groups.Item("cell")

        let sheet = if sheetGroup.Success then sheetGroup.Value else sheetContext
        let row, column = parseCellAddress cellGroup.Value

        { Sheet = sheet; Row = row; Column = column }

    ///Parses an excel range of the form
    ///<ADDRESS>:<ADDRESS> | <ADDRESS>
    ///ADDRESS is parsed with the parseExcelAddress function
    let parseExcelRange sheetContext (range : string) = 
        let addresses =
            range.Split(':')
            |> Array.map (parseExcelAddress sheetContext)
        match addresses with
        | [|a; b|] -> Bounded(a, b)
        | [|a|] -> Unbounded a
        | _ -> failwith (sprintf "A range can contain only one or two address [%s]" range)

    ///Parses a potential sequence of excel ranges, seperated by commas
    let parseExcelRanges sheetContext (range : string) =
        range.Split(',')
        |> Array.map (parseExcelRange sheetContext)
        |> Array.toList

    ///Reads the data in a range from a workbook
    ///Data is represented as an array of rows
    let getCellsForRange (workbook : DataSet) range =
    
        let (startRow, endRow), (startColumn, endColumn), sheet = 
            match range with
            | Bounded (topLeft, bottomRight) ->                 
                let sheet = workbook.Tables.[topLeft.Sheet] //we assume that both range addresses are on the same sheet
                (topLeft.Row, bottomRight.Row + 1), (topLeft.Column, bottomRight.Column + 1), sheet                
            | Unbounded (topLeft) -> 
                let sheet = workbook.Tables.[topLeft.Sheet]
                (topLeft.Row, sheet.Rows.Count), (topLeft.Column, sheet.Columns.Count), sheet
        
        seq { for irow in startRow .. endRow do
            let row = sheet.Rows.[irow]
            yield seq { for jcol in startColumn .. endColumn do yield row.[jcol] |> string } |> Seq.toArray }
        |> Seq.toArray

    let mergeData data =
        [|[|"DUDE"|]|]
        
    let public getCells (workbook : DataSet) range =
        let worksheets = workbook.Tables
        let firstWorkSheetName = worksheets.[0].TableName

        let ranges = parseExcelRanges firstWorkSheetName range

        match ranges |> List.map (getCellsForRange workbook) with
        | [data] -> data
        | data -> mergeData data