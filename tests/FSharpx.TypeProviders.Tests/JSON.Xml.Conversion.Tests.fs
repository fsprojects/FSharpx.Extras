module FSharpx.TypeProviders.Tests.JSON.XmlConversionTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit
open System.Xml.Linq

[<Test>]
let ``Can serialize JSON to XML``() =
    let text = """{"items": [{"id": "Open"}, null, {"id": 25}]}"""
    let json = parse text
    let xml = json.ToXml() |> Seq.head 
    let expectedXml = XDocument.Parse("<items><item id=\"Open\" /><item /><item id=\"25\" /></items>")
    xml.ToString() |> should equal (expectedXml.ToString())

[<Test>]
let ``Can serialize single XML node to JSON``() =
    let text = "<item name=\"Steffen\" />" 
    let xml = XDocument.Parse text
    let generatedJSON = fromXml xml
    let expectedJSON = """{"name":"Steffen"}"""
    generatedJSON.ToString() |> should equal expectedJSON

[<Test>]
let ``Can serialize XML to JSON``() =
    let text = "<items><item id=\"Open\" /><item /><item id=\"25\" /></items>"    
    let xml = XDocument.Parse text
    let generatedJSON = xml.ToJson()
    let expectedJSON = """{"items":[{"id":"Open"},{},{"id":"25"}]}"""
    generatedJSON.ToString() |> should equal expectedJSON