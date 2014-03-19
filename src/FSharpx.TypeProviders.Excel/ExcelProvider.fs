module FSharpx.TypeProviders.ExcelProvider

open System
open System.Collections.Generic
open System.Data
open System.IO
open System.Reflection

open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.ExcelAddressing
open ICSharpCode.SharpZipLib.Zip
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes

// Represents a row in a provided ExcelFileInternal
type Row(rowIndex, getCellValue: int -> int -> obj, columns: Map<string, int>) = 
    member this.GetValue columnIndex = getCellValue rowIndex columnIndex

    override this.ToString() =
        let columnValueList =                    
            [for column in columns do
                let value = getCellValue rowIndex column.Value
                let columnName, value = column.Key, string value
                yield sprintf "\t%s = %s" columnName value]
            |> String.concat "\n"

        sprintf "Row %d\n%s" rowIndex columnValueList

// get the type, and implementation of a getter property based on a template value
let internal propertyImplementation columnIndex (value : obj) =
    match value with
    | :? float -> typeof<double>, (fun [row] -> <@@ (%%row: Row).GetValue columnIndex |> (fun v -> v :?> double) @@>)
    | :? bool -> typeof<bool>, (fun [row] -> <@@ (%%row: Row).GetValue columnIndex |> (fun v -> v :?> bool) @@>)
    | :? DateTime -> typeof<DateTime>, (fun [row] -> <@@ (%%row: Row).GetValue columnIndex |> (fun v -> v :?> DateTime) @@>)
    | :? string -> typeof<string>, (fun [row] -> <@@ (%%row: Row).GetValue columnIndex |> (fun v -> v :?> string) @@>)
    | _ -> typeof<obj>, (fun [row] -> <@@ (%%row: Row).GetValue columnIndex @@>)            
         
// gets a list of column definition information for the columns in a view         
let internal getColumnDefinitions (data : View) forcestring =
    let getCell = getCellValue data
    [for columnIndex in 0 .. data.ColumnMappings.Count - 1 do
        let columnName = getCell 0 columnIndex |> string
        if not (String.IsNullOrWhiteSpace(columnName)) then
            let cellType, getter =
                if forcestring then
                    let getter = (fun [row] -> 
                    <@@ 
                        let value = (%%row: Row).GetValue columnIndex |> string
                        if String.IsNullOrEmpty value then null
                        else value
                    @@>)
                    typedefof<string>, getter
                else
                    let cellValue = getCell 1 columnIndex
                    propertyImplementation columnIndex cellValue             
            yield (columnName, (columnIndex, cellType, getter))]

// Simple type wrapping Excel data
type ExcelFileInternal(filename, range) =
    
    let data = 
        let view = openWorkbookView filename range
        let columns = [for (columnName, (columnIndex, _, _)) in getColumnDefinitions view true -> columnName, columnIndex] |> Map.ofList
        let buildRow rowIndex = new Row(rowIndex, getCellValue view, columns)        
        seq{ 1 .. view.RowCount}
        |> Seq.map buildRow

    member __.Data = data

type internal GlobalSingleton private () =
    static let mutable instance = Dictionary<_, _>()
    static member Instance = instance

let internal memoize f x =
    if (GlobalSingleton.Instance).ContainsKey(x) then (GlobalSingleton.Instance).[x]
    else 
        let res = f x
        (GlobalSingleton.Instance).[x] <- res
        res

let internal typExcel(cfg:TypeProviderConfig) =

   //Re
    let sharpZipLibAssemblyName = 
        let zipFileType = typedefof<ZipFile>
        zipFileType.Assembly.GetName()

    let loadedAssemblies = new HashSet<string>()
   
    let resolveAssembly sender (resolveEventArgs : ResolveEventArgs) =        
        let assemblyName = resolveEventArgs.Name
        if loadedAssemblies.Add( assemblyName ) then         
            let assemblyName =
               if assemblyName.StartsWith(sharpZipLibAssemblyName.Name)
               then sharpZipLibAssemblyName.FullName
               else assemblyName
            Assembly.Load( assemblyName )
        else null

    do 
        let handler = new ResolveEventHandler( resolveAssembly )
        AppDomain.CurrentDomain.add_AssemblyResolve handler

    let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()   

    // Create the main provided type
    let excelFileProvidedType = ProvidedTypeDefinition(executingAssembly, rootNamespace, "ExcelFile", Some(typeof<ExcelFileInternal>))

    // Parameterize the type by the file to use as a template
    let filename = ProvidedStaticParameter("filename", typeof<string>)
    let range = ProvidedStaticParameter("sheetname", typeof<string>, "")
    let forcestring = ProvidedStaticParameter("forcestring", typeof<bool>, false)
    let staticParams = [ filename; range; forcestring ]

    do excelFileProvidedType.DefineStaticParameters(staticParams, fun tyName paramValues ->
        let (filename, range, forcestring) = 
            match paramValues with
            | [| :? string  as filename;   :? string as range;  :? bool as forcestring|] -> (filename, range, forcestring)
            | [| :? string  as filename;   :? bool as forcestring |] -> (filename, String.Empty, forcestring)
            | [| :? string  as filename|] -> (filename, String.Empty, false)
            | _ -> ("no file specified to type provider", String.Empty,  true)

        // resolve the filename relative to the resolution folder
        let resolvedFilename = Path.Combine(cfg.ResolutionFolder, filename)

        let ProvidedTypeDefinitionExcelCall (filename, range, forcestring)  =         
            let data = openWorkbookView resolvedFilename range

            // define a provided type for each row, erasing to a int -> obj
            let providedRowType = ProvidedTypeDefinition("Row", Some(typeof<Row>))         

            // add one property per Excel field
            let columnProperties = getColumnDefinitions data forcestring
            for (columnName, (columnIndex, propertyType, getter)) in columnProperties do            

                let prop = ProvidedProperty(columnName, propertyType, GetterCode = getter)
                // Add metadata defining the property's location in the referenced file
                prop.AddDefinitionLocation(1, columnIndex, filename)
                providedRowType.AddMember(prop)

            // define the provided type, erasing to an seq<int -> obj>
            let providedExcelFileType = ProvidedTypeDefinition(executingAssembly, rootNamespace, tyName, Some(typeof<ExcelFileInternal>))

            // add a parameterless constructor which loads the file that was used to define the schema
            providedExcelFileType.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ ExcelFileInternal(resolvedFilename, range) @@>))

            // add a constructor taking the filename to load
            providedExcelFileType.AddMember(ProvidedConstructor([ProvidedParameter("filename", typeof<string>)], InvokeCode = fun [filename] -> <@@ ExcelFileInternal(%%filename, range) @@>))

            // add a new, more strongly typed Data property (which uses the existing property at runtime)
            providedExcelFileType.AddMember(ProvidedProperty("Data", typedefof<seq<_>>.MakeGenericType(providedRowType), GetterCode =fun [excFile] -> <@@ (%%excFile:ExcelFileInternal).Data @@>))

            // add the row type as a nested type
            providedExcelFileType.AddMember(providedRowType)

            providedExcelFileType

        (memoize ProvidedTypeDefinitionExcelCall)(filename, range, forcestring))

    // add the type to the namespace
    excelFileProvidedType

[<TypeProvider>]
type public ExcelProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(rootNamespace,[typExcel cfg])

[<TypeProviderAssembly>]
do ()