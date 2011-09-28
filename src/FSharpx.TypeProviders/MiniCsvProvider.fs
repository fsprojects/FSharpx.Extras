// Copyright (c) Microsoft Corporation 2005-2011.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

module FSharpx.TypeProviders.MiniCsvProvider

open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open System.Reflection
open System.IO
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Text.RegularExpressions

// Simple type wrapping CSV data
type CsvFile(filename) =
    // Cache the sequence of all data lines (all lines but the first)
    let data = 
        seq { for line in File.ReadAllLines(filename) |> Seq.skip 1 do
                yield line.Split(',') |> Array.map float }
        |> Seq.cache
    member __.Data = data

// Create the main provided type
let csvType (cfg:TypeProviderConfig) =
    erasedType<obj> thisAssembly rootNamespace "MinCsv"
    |> staticParameter "filename"
        (fun typeName fileName ->
            // resolve the filename relative to the resolution folder
            let resolvedFilename = Path.Combine(cfg.ResolutionFolder, fileName)
        
            // get the first line from the file
            let headerLine = File.ReadLines(resolvedFilename) |> Seq.head

            // define a provided type for each row, erasing to a float[]
            let rowTy = ProvidedTypeDefinition("Row", Some(typeof<float[]>))

            // extract header names from the file, splitting on commas
            // we use Regex matching so that we can get the position in the row at which the field occurs
            let headers = Regex.Matches(headerLine, "[^,]+")

            // add one property per CSV field
            for i in 0 .. headers.Count - 1 do
                let headerText = headers.[i].Value
            
                // try to decompose this header into a name and unit
                let fieldName, fieldTy =
                    let m = Regex.Match(headerText, @"(?<field>.+) \((?<unit>.+)\)")
                    if m.Success then
                        let unitName = m.Groups.["unit"].Value
                        let units = ProvidedMeasureBuilder.Default.SI unitName
                        m.Groups.["field"].Value, ProvidedMeasureBuilder.Default.AnnotateType(typeof<float>, [units])
                    else
                        // no units, just treat it as a normal float
                        headerText, typeof<float>
                let prop = ProvidedProperty(fieldName, fieldTy, GetterCode = fun [row] -> <@@ (%%row:float[]).[i] @@>)

                // Add metadata defining the property's location in the referenced file
                prop.AddDefinitionLocation(1, headers.[i].Index + 1, fileName)
                rowTy.AddMember(prop)
                
            // define the provided type, erasing to CsvFile
            let ty =
                erasedType<CsvFile> thisAssembly rootNamespace typeName 
                |> addXmlDoc (sprintf "A strongly typed interface to the csv file '%s'" fileName)
                |> addMember (
                    provideConstructor
                        [] 
                        (fun _ -> <@@ CsvFile(resolvedFilename) @@>)
                    |> addXmlDoc "Initializes a CsvFile instance")
                |> addMember (
                    provideConstructor
                        ["filename", typeof<string>] 
                        (fun args -> <@@ CsvFile(%%args.[0]) @@>)
                    |> addXmlDoc "Initializes a CsvFile instance from the given path.")
            
            // add a new, more strongly typed Data property (which uses the existing property at runtime)
            ty.AddMember(ProvidedProperty("Data", typedefof<seq<_>>.MakeGenericType(rowTy), GetterCode = fun [csvFile] -> <@@ (%%csvFile:CsvFile).Data @@>))

            // add the row type as a nested type
            ty.AddMember(rowTy)
            ty)