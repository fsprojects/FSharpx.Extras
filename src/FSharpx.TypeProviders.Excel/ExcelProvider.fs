module FSharpx.TypeProviders.ExcelProvider

open System
open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open FSharpx.TypeProviders.Helper
open System.Collections.Generic
open System.Data
open System
open FSharpx.TypeProviders.ExcelAddressing

// Simple type wrapping Excel data
type ExcelFileInternal(filename, range) =
    let data  = 
        let view = openWorkbookView filename range
        seq{ 1 .. view.RowCount}
        |> Seq.map (fun row -> getCellValue view row)

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

   let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()   
   
   // Create the main provided type
   let excelFileProvidedType = ProvidedTypeDefinition(executingAssembly, rootNamespace, "ExcelFile", Some(typeof<ExcelFileInternal>))

   // Parameterize the type by the file to use as a template
   let filename = ProvidedStaticParameter("filename", typeof<string>)
   let range = ProvidedStaticParameter("sheetname", typeof<string>, "Sheet1")
   let forcestring = ProvidedStaticParameter("forcestring", typeof<bool>, false)
   let staticParams = [ filename; range; forcestring ]

   do excelFileProvidedType.DefineStaticParameters(staticParams, fun tyName paramValues ->
      let (filename, range, forcestring) = 
            match paramValues with
            | [| :? string  as filename;   :? string as range;  :? bool as forcestring|] -> (filename, range, forcestring)
            | [| :? string  as filename;   :? bool as forcestring |] -> (filename, "Sheet1", forcestring)
            | [| :? string  as filename|] -> (filename, "Sheet1", false)
            | _ -> ("no file specified to type provider", "",  true)

      // resolve the filename relative to the resolution folder
      let resolvedFilename = Path.Combine(cfg.ResolutionFolder, filename)

      let ProvidedTypeDefinitionExcelCall (filename, range, forcestring)  =         
         let data = openWorkbookView resolvedFilename range

         // define a provided type for each row, erasing to a int -> obj
         let providedRowType = ProvidedTypeDefinition("Row", Some(typeof<int -> obj>))
         
         // add one property per Excel field
         let getCell = getCellValue data         
         for column in 0 .. data.ColumnMappings.Count - 1 do            
            let header = getCell 0 column |> string
            if not (String.IsNullOrWhiteSpace(header)) then do
                let firstValue = getCell 1 column
                let readString = (fun [rowCellGetter] -> <@@ (%%rowCellGetter: int -> obj) column |> string @@>)
                let readFloat = (fun [rowCellGetter] -> <@@ (%%rowCellGetter: int -> obj) column |> (fun v -> v :? float) @@>)
                let valueType, gettercode  = 
                   if  forcestring then
                      typeof<string>, readString
                   else
                      match firstValue with
                      | :? string -> typeof<string>, readString
                      | :? float -> typeof<float> , readFloat
                      |_ -> typeof<string>, readString

                let prop = ProvidedProperty(header, valueType, GetterCode = gettercode)
                // Add metadata defining the property's location in the referenced file
                prop.AddDefinitionLocation(1, column, filename)
                providedRowType.AddMember(prop)

         // define the provided type, erasing to an seq<int -> obj>
         let providedExcelFileType = ProvidedTypeDefinition(executingAssembly, rootNamespace, tyName, Some(typeof<ExcelFileInternal>))

         // add a parameterless constructor which loads the file that was used to define the schema
         providedExcelFileType.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ ExcelFileInternal(resolvedFilename, range) @@>))

         // add a constructor taking the filename to load
         providedExcelFileType.AddMember(ProvidedConstructor([ProvidedParameter("filename", typeof<string>)], InvokeCode = fun [filename] -> <@@  ExcelFileInternal(%%filename, range) @@>))

         // add a new, more strongly typed Data property (which uses the existing property at runtime)
         providedExcelFileType.AddMember(ProvidedProperty("Data", typedefof<seq<_>>.MakeGenericType(providedRowType), GetterCode =fun [excFile] -> <@@ (%%excFile:ExcelFileInternal).Data @@>))

         // add the row type as a nested type
         providedExcelFileType.AddMember(providedRowType)
         
         providedExcelFileType

      (memoize ProvidedTypeDefinitionExcelCall)(filename, range, forcestring)
      )

   // add the type to the namespace
   excelFileProvidedType

[<TypeProvider>]
type public ExcelProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(rootNamespace,[typExcel cfg])

[<TypeProviderAssembly>]
do ()