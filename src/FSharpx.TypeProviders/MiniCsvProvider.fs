// Copyright (c) Microsoft Corporation 2005-2011.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

module FSharpx.TypeProviders.MiniCsvProvider

open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open System.Reflection
open System.IO
open Samples.FSharp.ProvidedTypes
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
            let resolvedFileName = findConfigFile cfg.ResolutionFolder fileName
            let headerLine =
                resolvedFileName
                    |> File.ReadLines
                    |> Seq.head

            // extract header names from the file, splitting on commas
            // we use Regex matching so that we can get the position in the row at which the field occurs
            let headers = [for m in Regex.Matches(headerLine, "[^,]+") -> m]

            let rowType =
                runtimeType<float[]> "Row"
                  |> hideOldMethods
                  |++!> (
                        headers
                        |> Seq.mapi (fun i header ->
                            // try to decompose this header into a name and unit
                            let fieldName, fieldType =
                                let m = Regex.Match(header.Value, @"(?<field>.+) \((?<unit>.+)\)")
                                if m.Success then
                                    let unitName = m.Groups.["unit"].Value
                                    let units = ProvidedMeasureBuilder.Default.SI unitName
                                    m.Groups.["field"].Value, ProvidedMeasureBuilder.Default.AnnotateType(typeof<float>, [units])
                                else
                                    // no units, just treat it as a normal float
                                    header.Value, typeof<float>
                            
                            provideProperty 
                                fieldName 
                                fieldType
                                (fun args -> <@@ (%%args.[0]:float[]).[i] @@>)
                              |> addDefinitionLocation 
                                    { Line = 1
                                      Column = header.Index + 1 
                                      FileName = fileName}))
                
            // define the provided type, erasing to CsvFile
            erasedType<CsvFile> thisAssembly rootNamespace typeName 
            |> addXmlDoc (sprintf "A strongly typed interface to the csv file '%s'" fileName)
            |+!> (provideConstructor
                    [] 
                    (fun _ -> <@@ CsvFile(resolvedFileName) @@>)
                |> addXmlDoc "Initializes a CsvFile instance")
            |+!> (provideConstructor
                    ["filename", typeof<string>] 
                    (fun args -> <@@ CsvFile(%%args.[0]) @@>)
                |> addXmlDoc "Initializes a CsvFile instance from the given path.")
            |+!> provideProperty
                    "Data"
                    (typedefof<seq<_>>.MakeGenericType rowType)
                    (fun args -> <@@ (%%args.[0]:CsvFile).Data @@>)
            |+!> rowType)