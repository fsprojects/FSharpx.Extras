module FSharpx.TypeProviders.ExcelProvider

open System.Reflection
open System.IO
open System
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.Office.Interop
open System.Diagnostics

module Utils =
   let ApplyMoveToRange (rg:Excel.Range) (move:Excel.XlDirection) = 
      rg.Worksheet.Range(rg, rg.End(move))


// Simple type wrapping Excel data
type  ExcelFileInternal(filename, sheetorrangename) =
      let data  = 
         let xlApp = new Excel.ApplicationClass()
         xlApp.Visible <- false
         xlApp.ScreenUpdating <- false
         xlApp.DisplayAlerts <- false;
         let xlWorkBookInput = xlApp.Workbooks.Open(filename)



         let mysheets = seq { for  sheet in xlWorkBookInput.Worksheets do yield sheet :?> Excel.Worksheet }
         let names = seq { for name in xlWorkBookInput.Names do yield name :?> Excel.Name}

         let hasWs =   Seq.exists (fun (ws:Excel.Worksheet) -> (ws.Name = sheetorrangename)) mysheets 
         let xlRangeInput = if hasWs  then 
                                 let sheet = Seq.find (fun (ws:Excel.Worksheet) -> (ws.Name = sheetorrangename)) mysheets
                                 let firstcell = sheet.Cells.Item(1,1) :?> Excel.Range
                                 Utils.ApplyMoveToRange (Utils.ApplyMoveToRange firstcell Excel.XlDirection.xlToRight) Excel.XlDirection.xlDown
                            else
                              let hasName =   Seq.exists (fun (ws:Excel.Name) -> (ws.Name = sheetorrangename)) names 
                              if hasName then
                                 (Seq.find (fun (ws:Excel.Name) -> (ws.Name = sheetorrangename)) names ).RefersToRange
                              else
                                 failwith (sprintf "Sheet or range %A was not found" sheetorrangename)

         let rows_data = seq { for row  in xlRangeInput.Rows do 
                                 yield row :?> Excel.Range } |> Seq.skip 1
         let res = 
            seq { for line_data in rows_data do 
                  yield ( seq { for cell in line_data.Columns do
                                 yield (cell  :?> Excel.Range ).Value2} 
                           |> Seq.toArray
                        )
               }
               |> Seq.toArray
         xlWorkBookInput.Close()
         xlApp.Quit()
         res

      member __.Data = data

type internal ReflectiveBuilder = 
   static member Cast<'a> (args:obj) =
      args :?> 'a
   static member BuildTypedCast lType (args: obj) = 
         typeof<ReflectiveBuilder>
            .GetMethod("Cast")
            .MakeGenericMethod([|lType|])
            .Invoke(null, [|args|])

[<TypeProvider>]
type public ExcelProvider(cfg:TypeProviderConfig) as this =
   inherit TypeProviderForNamespaces()

   // Get the assembly and namespace used to house the provided types
   let asm = System.Reflection.Assembly.GetExecutingAssembly()
   let ns = "FSharpx.TypeProviders.ExcelProvider"

   // Create the main provided type
   let excTy = ProvidedTypeDefinition(asm, ns, "ExcelFile", Some(typeof<obj>))

   // Parameterize the type by the file to use as a template
   let filename = ProvidedStaticParameter("filename", typeof<string>)
   let sheetorrangename = ProvidedStaticParameter("sheetname", typeof<string>, "Sheet1")
   let forcestring = ProvidedStaticParameter("forcestring", typeof<bool>, false)

   let staticParams = [ filename
                        sheetorrangename   
                        forcestring]

   do excTy.DefineStaticParameters(staticParams, fun tyName paramValues ->
      let (filename, sheetorrangename ,  forcestring) = 
                                    match paramValues with
                                    | [| :? string  as filename;   :? string as sheetorrangename   ;  :? bool as forcestring |] -> (filename, sheetorrangename  , forcestring)
                                    | [| :? string  as filename;   :? bool as forcestring |] -> (filename, "Sheet1",forcestring)
                                    | [| :? string  as filename|] -> (filename, "Sheet1", false)
                                    | _ -> ("no file specified to type provider", "",  true)

         // [| :? string as filename ,  :? bool  as forcestring |]
         // resolve the filename relative to the resolution folder
      let resolvedFilename = Path.Combine(cfg.ResolutionFolder, filename)
        
      let xlApp = new Excel.ApplicationClass()
      let xlWorkBookInput = xlApp.Workbooks.Open(resolvedFilename)
      let mysheets = seq { for  sheet in xlWorkBookInput.Worksheets do yield sheet :?> Excel.Worksheet }
      let names = seq { for name in xlWorkBookInput.Names do yield name :?> Excel.Name}


      let hasWs =   Seq.exists (fun (ws:Excel.Worksheet) -> (ws.Name = sheetorrangename)) mysheets 
      let xlRangeInput =   if hasWs  then 
                              let sheet = Seq.find (fun (ws:Excel.Worksheet) -> (ws.Name = sheetorrangename)) mysheets
                              let firstcell = sheet.Cells.Item(1,1) :?> Excel.Range
                              Utils.ApplyMoveToRange (Utils.ApplyMoveToRange firstcell Excel.XlDirection.xlToRight) Excel.XlDirection.xlDown
                           else
                              let hasName =   Seq.exists (fun (ws:Excel.Name) -> (ws.Name = sheetorrangename)) names 
                              if hasName then
                                 (Seq.find (fun (ws:Excel.Name) -> (ws.Name = sheetorrangename)) names ).RefersToRange
                              else
                                 failwith (sprintf "Sheet or range %A was not found" sheetorrangename)


      let lines = (seq { for row in xlRangeInput.Rows do yield row } |> Seq.cache)
      let headerLine =  (Seq.head   lines):?> Excel.Range
      // define a provided type for each row, erasing to a float[]
      let rowTy = ProvidedTypeDefinition("Row", Some(typeof<obj[]>))


      let oFirstdataLine  =  
         match (Seq.length lines) with
            | 1 -> None
            | _  -> Some( lines |> Seq.skip 1 |> Seq.head :?> Excel.Range)
            

      // add one property per Excel field
      for i in 0 .. (headerLine.Columns.Count - 1 ) do
         let headerText = ((headerLine.Cells.Item(1,i+1) :?> Excel.Range).Value2).ToString()
            
         let valueType, gettercode  = 
            if  forcestring || oFirstdataLine   = None then
               typeof<string>, (fun [row] -> <@@ ((%%row:obj[]).[i]):?> string  @@>)
            else
               let firstdataLine = oFirstdataLine.Value
               if xlApp.WorksheetFunction.IsText(firstdataLine.Cells.Item(1,i+1)) then
                  typeof<string>, (fun [row] -> <@@ ((%%row:obj[]).[i]):?> string  @@>)
               elif  xlApp.WorksheetFunction.IsNumber(firstdataLine.Cells.Item(1,i+1)) then
                  typeof<float> , (fun [row] -> <@@ ((%%row:obj[]).[i]):?> float  @@>)
               else
                  typeof<string>, (fun [row] -> <@@ ((%%row:obj[]).[i]):?> string  @@>)

         //TODO : test w different types
         let prop = ProvidedProperty(headerText, valueType, GetterCode = gettercode)
         // Add metadata defining the property's location in the referenced file
         prop.AddDefinitionLocation(1, i, filename)
         rowTy.AddMember(prop)

      xlWorkBookInput.Close()
      xlApp.Quit()
      // define the provided type, erasing to excelFile
      let ty = ProvidedTypeDefinition(asm, ns, tyName, Some(typeof<ExcelFileInternal>))

      // add a parameterless constructor which loads the file that was used to define the schema
      ty.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ ExcelFileInternal(resolvedFilename, sheetorrangename) @@>))
      // add a constructor taking the filename to load
      ty.AddMember(ProvidedConstructor([ProvidedParameter("filename", typeof<string>)], InvokeCode = fun [filename] -> <@@  ExcelFileInternal(%%filename) @@>))
      // add a new, more strongly typed Data property (which uses the existing property at runtime)
      ty.AddMember(ProvidedProperty("Data", typedefof<seq<_>>.MakeGenericType(rowTy), GetterCode = fun [excFile] -> <@@ (%%excFile:ExcelFileInternal).Data @@>))
      // add the row type as a nested type
      ty.AddMember(rowTy)
      ty)

   // add the type to the namespace
   do this.AddNamespace(ns, [excTy])

[<TypeProviderAssembly>]
do()