// Copyright (c) Microsoft Corporation 2005-2011.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 
module FSharpx.TypeProviders.MiniCsvProvider

open FSharpx.TypeProviders.Helper
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
let internal csvType ownerType (cfg:TypeProviderConfig) =
    let csvType = erasedType<obj> thisAssembly rootNamespace "MinCsv"
    csvType.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("filename", typeof<string>)], 
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as fileName |] -> 
                let resolvedFileName = findConfigFile cfg.ResolutionFolder fileName
                watchForChanges ownerType resolvedFileName
                let headerLine =
                    resolvedFileName
                        |> File.ReadLines
                        |> Seq.head

                // extract header names from the file, splitting on commas
                // we use Regex matching so that we can get the position in the row at which the field occurs
                let headers = [for m in Regex.Matches(headerLine, "[^,]+") -> m]

                let rowType = runtimeType<float[]> "Row"
                rowType.HideObjectMethods <- true

                headers
                    |> Seq.iteri (fun i header ->
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
                            
                            let property = 
                                ProvidedProperty (
                                    propertyName = fieldName,
                                    propertyType = fieldType,
                                    GetterCode = (fun args -> <@@ (%%args.[0]:float[]).[i] @@>))

                            property.AddDefinitionLocation(1,header.Index + 1,fileName)
                            rowType.AddMember property)
                
                // define the provided type, erasing to CsvFile
                let csvType = erasedType<CsvFile> thisAssembly rootNamespace typeName 
                csvType.AddXmlDoc(sprintf "A strongly typed interface to the csv file '%s'" fileName)

                let defaultConstructor = 
                    ProvidedConstructor(
                        parameters = [],
                        InvokeCode = (fun _ -> <@@ CsvFile(resolvedFileName) @@>))
                defaultConstructor.AddXmlDoc "Initializes a CsvFile instance"

                csvType.AddMember defaultConstructor

                let fileNameConstructor = 
                    ProvidedConstructor(
                        parameters = [ProvidedParameter("filename", typeof<string>)],
                        InvokeCode = (fun args -> <@@ CsvFile(%%args.[0]) @@>))
                fileNameConstructor.AddXmlDoc "Initializes a CsvFile instance from the given path."

                csvType.AddMember fileNameConstructor

                let rootProperty =
                    ProvidedProperty(
                        propertyName = "Data",
                        propertyType = seqType rowType,
                        GetterCode =  (fun args -> <@@ (%%args.[0]:CsvFile).Data @@>))

                rootProperty.AddXmlDoc "Gets the document root"

                csvType.AddMember rootProperty
                csvType.AddMember rowType
                csvType))

    csvType