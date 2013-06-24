module internal FSharpx.TypeProviders.ExcelAddressing
    open System
    open System.Data
    open System.Text.RegularExpressions
    open System.IO   
        
    type Address = {
        Sheet: string;
        Row: int;
        Column: int}

    type Range =
        | Bounded of Address * Address
        | Unbounded of Address

    type RangeView = {
        StartColumn: int;
        EndColumn: int;
        StartRow: int;
        EndRow: int;
        Sheet: DataTable }    

    type View = {
        RowCount: int;
        ColumnMappings: Map<int, int * RangeView> }

    ///Parses an excel row column address of the form <COLUMN LETTERS><ROW NUMBER> into a zero based row, column address.
    ///For example the address A1 would be parsed as 0, 0
    let parseCellAddress cellAddress =

        if String.IsNullOrWhiteSpace cellAddress then 0, 0
        else
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

    let addressParser = new Regex("((?<sheet>[^!]*)!)?(?<cell>\w+\d+)?")

    ///Parses an excel address from a string
    ///Valid inputs look like:
    ///Sheet!A1
    ///B3
    let parseExcelAddress sheetContext address =
        if address <> sheetContext then
            let regexMatch = addressParser.Match(address)
            let sheetGroup = regexMatch.Groups.Item("sheet")
            let cellGroup = regexMatch.Groups.Item("cell")

            let sheet = if sheetGroup.Success then sheetGroup.Value else sheetContext
            let row, column = parseCellAddress cellGroup.Value

            { Sheet = sheet; Row = row; Column = column }
        else { Sheet = sheetContext; Row = 0; Column = 0 }

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

    ///Gets the start and end offsets of a range
    let getRangeView (workbook : DataSet) range =
        let topLeft, bottomRight, sheet =
            match range with
            | Bounded (topLeft, bottomRight) ->
                let sheet = workbook.Tables.[topLeft.Sheet]
                topLeft, bottomRight, sheet                
            | Unbounded (topLeft) ->
                let sheet = workbook.Tables.[topLeft.Sheet]
                topLeft, {topLeft with Row = sheet.Rows.Count; Column = sheet.Columns.Count - 1}, sheet
                
        { StartColumn = topLeft.Column; StartRow = topLeft.Row; EndColumn = bottomRight.Column; EndRow = bottomRight.Row; Sheet = sheet }
        
    ///Gets a View object which can be used to read data from the given range in the DataSet
    let public getView (workbook : DataSet) range =
        let worksheets = workbook.Tables
        let firstWorkSheetName = worksheets.[0].TableName
        
        let ranges = 
            parseExcelRanges firstWorkSheetName range
            |> List.map (getRangeView workbook)

        let minRow = ranges |> Seq.map (fun range -> range.StartRow) |> Seq.min
        let maxRow = ranges |> Seq.map (fun range -> range.EndRow) |> Seq.max
        let rowCount = maxRow - minRow

        let rangeViewOffsetRecord rangeView =
            seq{ rangeView.StartColumn .. rangeView.EndColumn }
            |> Seq.map (fun i -> i, rangeView)
            |> Seq.toList

        let rangeViewsByColumn =
            ranges
            |> Seq.map rangeViewOffsetRecord
            |> Seq.concat
            |> Seq.toList

        if rangeViewsByColumn |> Seq.distinctBy fst |> Seq.length < rangeViewsByColumn.Length then
            failwith "Ranges cannot overlap"

        let columns =
            rangeViewsByColumn
            |> Seq.mapi (fun index entry -> (index, entry))
            |> Map.ofSeq

        { RowCount = rowCount; ColumnMappings = columns }

    ///Reads the value of a cell from a view
    let getCellValue view row column = 
        let columns = view.ColumnMappings
        let sheetColumn, rangeView = columns.[column]
        let row = rangeView.StartRow + row
        let sheet = rangeView.Sheet
        if row < sheet.Rows.Count && sheetColumn < sheet.Columns.Count then rangeView.Sheet.Rows.[row].Item(sheetColumn)
        else null

    ///Reads the contents of an excel file into a DataSet
    let public openWorkbookView filename range =            
        use stream = File.OpenRead(filename)
        let excelReader = 
            if filename.EndsWith(".xlsx") then Excel.ExcelReaderFactory.CreateOpenXmlReader(stream)
            else Excel.ExcelReaderFactory.CreateBinaryReader(stream)

        let workbook = excelReader.AsDataSet()
        getView workbook range
        