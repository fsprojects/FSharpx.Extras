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
type  ExcelFileInternal(filename, range) =
    let data = openWorkbookView filename range
    member internal __.Data = data

type internal ReflectiveBuilder = 
   static member Cast<'a> (args:obj) =
      args :?> 'a
   static member BuildTypedCast lType (args: obj) = 
         typeof<ReflectiveBuilder>
            .GetMethod("Cast")
            .MakeGenericMethod([|lType|])
            .Invoke(null, [|args|])

type internal GlobalSingleton private () =
   static let mutable instance = Dictionary<_, _>()
   static member Instance = instance

let internal memoize f =
      //let cache = Dictionary<_, _>()
      fun x ->
         if (GlobalSingleton.Instance).ContainsKey(x) then (GlobalSingleton.Instance).[x]
         else let res = f x
              (GlobalSingleton.Instance).[x] <- res
              res


let internal typExcel(cfg:TypeProviderConfig) =
   // Create the main provided type
   let excTy = ProvidedTypeDefinition(System.Reflection.Assembly.GetExecutingAssembly(), rootNamespace, "ExcelFile", Some(typeof<obj>))

   // Parameterize the type by the file to use as a template
   let filename = ProvidedStaticParameter("filename", typeof<string>)
   let range = ProvidedStaticParameter("sheetname", typeof<string>, "Sheet1")
   let forcestring = ProvidedStaticParameter("forcestring", typeof<bool>, false)
   let staticParams = [ filename; range; forcestring ]

   do excTy.DefineStaticParameters(staticParams, fun tyName paramValues ->
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

         // define a provided type for each row, erasing to a float[]
         let rowTy = ProvidedTypeDefinition("Row", Some(typeof<obj[]>))         

         // add one property per Excel field
         for i in 0 .. data.Columns - 1 do
            let header = data.Item 0 i            
            if header <> null then do
                let headerText = header.ToString()
            
                let valueType, gettercode  = 
                   if  forcestring then
                      typeof<string>, (fun [row] -> <@@ ((%%row:obj[]).[i]) |> string  @@>)
                   else
                      match header with
                      | :? string -> typeof<string>, (fun [row] -> <@@ ((%%row:obj[]).[i]) |> string  @@>)
                      | :? float -> typeof<float> , (fun [row] -> <@@ ((%%row:obj[]).[i]) :?> float  @@>)
                      |_ -> typeof<string>, (fun [row] -> <@@ ((%%row:obj[]).[i]) |> string  @@>)

                //TODO : test w different types
                let prop = ProvidedProperty(headerText, valueType, GetterCode = gettercode)
                // Add metadata defining the property's location in the referenced file
                prop.AddDefinitionLocation(1, i, filename)
                rowTy.AddMember(prop)

         // define the provided type, erasing to excelFile
         let ty = ProvidedTypeDefinition(System.Reflection.Assembly.GetExecutingAssembly(), rootNamespace, tyName, Some(typeof<ExcelFileInternal>))

         // add a parameterless constructor which loads the file that was used to define the schema
         ty.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ ExcelFileInternal(resolvedFilename, range) @@>))
         // add a constructor taking the filename to load
         ty.AddMember(ProvidedConstructor([ProvidedParameter("filename", typeof<string>)], InvokeCode = fun [filename] -> <@@  ExcelFileInternal(%%filename, range) @@>))
         // add a new, more strongly typed Data property (which uses the existing property at runtime)
         ty.AddMember(ProvidedProperty("Data", typedefof<seq<_>>.MakeGenericType(rowTy), GetterCode = fun [excFile] -> <@@ (%%excFile:ExcelFileInternal).Data @@>))
         // add the row type as a nested type
         ty.AddMember(rowTy)
         ty

      (memoize ProvidedTypeDefinitionExcelCall)(filename, range, forcestring)
      )

   // add the type to the namespace
   excTy

[<TypeProvider>]
type public ExcelProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(rootNamespace,[typExcel cfg])

[<TypeProviderAssembly>]
do ()