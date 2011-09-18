module FSharpx.Tests.JSONWriterTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit


[<Test>] 
let ``Can serialize empty empty document``() = 
    emptyJObject.ToString()
    |> should equal "{}"

[<Test>] 
let ``Can serialize document with single property``() =
    (emptyJObject |> addProperty "firstName" (Text "John")).ToString()
    |> should equal "{\"firstName\":\"John\"}"


[<Test>] 
let ``Can serialize document with booleans``() =
    (emptyJObject |> addProperty "aa" (Boolean true) |> addProperty "bb" (Boolean false)).ToString()
    |> should equal "{\"aa\":true,\"bb\":false}"

[<Test>]
let ``Can serialize document with array, null and number``() =
    let text = "{\"items\":[{\"id\":\"Open\"},null,{\"id\":25}]}"
    let json = parse text
    json.ToString() |> should equal text