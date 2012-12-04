module FSharpx.Tests.JSON.SerializationTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit

[<Test>]
let ``Can serialize empty document``() = 
    emptyObject.ToString()
    |> should equal "{}"

[<Test>] 
let ``Can serialize document with single property``() =
    (emptyObject |> addStringProperty "firstName" "John").ToString()
    |> should equal "{\"firstName\":\"John\"}"

[<Test>] 
let ``Can serialize document with booleans``() =
    (emptyObject |> addBoolProperty "aa" true |> addBoolProperty "bb" false).ToString()
    |> should equal "{\"aa\":true,\"bb\":false}"

[<Test>]
let ``Can serialize document with array, null and number``() =
    let text = "{\"items\":[{\"id\":\"Open\"},null,{\"id\":25}]}"
    let json = parse text
    json.ToString() |> should equal text


[<Test>]
let ``Quotes in strings are property escaped``() = 
    let jsonStr = "{\"short_description\":\"This a string with \\\"quotes\\\"\"}"
    let j = parse jsonStr
    j.ToString() |> should equal jsonStr