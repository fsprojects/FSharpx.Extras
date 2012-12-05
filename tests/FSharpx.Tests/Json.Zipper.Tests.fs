module FSharpx.Tests.JSON.Zipper.Tests

open NUnit.Framework
open FSharpx.JSON
open FSharpx.JSON.Zipper
open FsUnit

[<Test>]
let ``Can modify the first property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> zipper 
    |> down
    |> modifyDecimal (decimal 26)
    |> getJson
    |> serialize 
    |> should equal "{\"age\":26,\"firstName\":\"John\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify the first text property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> zipper 
    |> down
    |> right
    |> modifyText "Johnny"
    |> getJson
    |> serialize 
    |> should equal "{\"age\":25,\"firstName\":\"Johnny\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify a text property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> zipper 
    |> downToProperty "firstName"
    |> modifyText "Johnny"
    |> getJson
    |> serialize 
    |> should equal "{\"age\":25,\"firstName\":\"Johnny\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify a bool property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\",\"isCool\":true}"
    |> zipper 
    |> downToProperty "isCool"
    |> modifyBool false
    |> getJson
    |> serialize 
    |> should equal "{\"age\":25,\"firstName\":\"John\",\"isCool\":false,\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify a decimal property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> zipper 
    |> downToProperty "age"
    |> modifyDecimal (decimal 26)
    |> getJson
    |> serialize 
    |> should equal "{\"age\":26,\"firstName\":\"John\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify an array element a simple document``() = 
    parse "[1, 2, 3]"
    |> zipper 
    |> down
    |> right
    |> modifyDecimal (decimal 26)
    |> getJson
    |> serialize 
    |> should equal "[1,26,3]"