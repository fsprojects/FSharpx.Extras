module FSharpx.Tests.JSON.SerializationTests

open NUnit.Framework
open FSharpx.JSON
open FSharpx.JSON.DocumentExtensions
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
    |> should equal "{\"firstName\":\"John\"}"

[<Test>] 
let ``Can serialize document with booleans``() =
    JObject
      .New()
      .AddBoolProperty("aa",true)
      .AddBoolProperty("bb",false)
      .ToString()
    |> should equal "{\"aa\":true,\"bb\":false}"

[<Test>]
let ``Can serialize document with array, null and number``() =
    let text = "{\"items\":[{\"id\":\"Open\"},null,{\"id\":25}]}"
    let json = parse text
    json.ToString() |> should equal text

[<Test>]
let ``Can serialise document with dates``() =
    JObject
        .New()
        .AddDateProperty("dd", new System.DateTime(1997, 2, 15, 0, 0, 0, System.DateTimeKind.Utc))
        .ToString()
    |> should equal "{\"dd\":\"1997-02-15T00:00:00.0000000Z\"}"