module FSharp.TypeProviders.Tests.JSON.SerializationTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit

[<Test>]
let ``Can serialize empty document``() = 
    JObject.New().ToString()
    |> should equal "{}"

[<Test>] 
let ``Can serialize document with single property``() =
    JObject
      .New()
      .AddTextProperty("firstName","John")
      .ToString()
    |> should equal """{"firstName":"John"}"""

[<Test>] 
let ``Can serialize document with booleans``() =
    JObject
      .New()
      .AddBoolProperty("aa",true)
      .AddBoolProperty("bb",false)
      .ToString()
    |> should equal """{"aa":true,"bb":false}"""

[<Test>]
let ``Can serialize document with array, null and number``() =
    let text = """{"items":[{"id":"Open"},null,{"id":25}]}"""
    let json = parse text
    json.ToString() |> should equal text