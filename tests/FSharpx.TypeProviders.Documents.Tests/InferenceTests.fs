module FSharpx.TypeProviders.Tests.JSON.InferenceTests

open NUnit.Framework
open FSharpx
open FsUnit

//type XmlCellSet = StructuredXml<Schema="""<CellSet><Row key="base64"><Cell timestamp="1349209076311" column="cGw6eG1s">base64</Cell></Row></CellSet>""">
//
//type JsonCellSet = StructuredJSON<Schema="""{"Row":[{"key":"base64","Cell":[{"timestamp":1349209076311,"column":"cGw6eG1s","$":"base64"}]}]}""">
//
//[<Test>]
//let ``Can infer the XML timestamp as integer``() = 
//    let inlined = XmlCellSet().Root
//    for row in inlined.GetRows() do
//        for cell in row.GetCells() do
//            Assert.AreEqual(1349209076311L,cell.Timestamp)
//            Assert.AreEqual(typeof<int64>,cell.Timestamp.GetType())            
//
//[<Test>]
//let ``Can infer the JSON timestamp as integer``() = 
//    let inlined = JsonCellSet().Root
//    for row in inlined.GetRows() do
//        for cell in row.GetCells() do
//            Assert.AreEqual(1349209076311L,cell.Timestamp)
//            Assert.AreEqual(typeof<int64>,cell.Timestamp.GetType())          