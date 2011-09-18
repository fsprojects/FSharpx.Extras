module FSharpx.Tests.JSONParserTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit


[<Test>] 
let ``Can parse empty document``() = 
    parse "{}" 
    |> should equal emptyJObject

[<Test>] 
let ``Can parse document with single property``() =
    parse "{\"firstName\": \"John\"}" 
    |> should equal (emptyJObject |> addProperty "firstName" (Text "John"))


[<Test>] 
let ``Can parse document with text and numbers``() =
    parse "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"age\": 25}" 
    |> should equal 
        (emptyJObject 
            |> addProperty "firstName" (Text "John")
            |> addProperty "lastName" (Text "Smith")
            |> addProperty "age" (Number 25.))

[<Test>]
let ``Can parse nested document`` () =
    "{
        \"main\": {
            \"title\": \"example\",
            \"nested\": {
                \"nestedTitle\": \"sub\"
            }
        }
    }"
    |> parse
    |> should equal 
            (emptyJObject
            |> addProperty "main"
                (emptyJObject
                    |> addProperty "title" (Text "example")
                    |> addProperty "nested" (emptyJObject |> addProperty "nestedTitle" (Text "sub"))))

[<Test>] 
let ``Can parse document with booleans``() =
    parse "{\"hasTrue\": true, \"hasFalse\": false }" 
    |> should equal 
        (emptyJObject 
            |> addProperty "hasTrue" (Boolean true)
            |> addProperty "hasFalse" (Boolean false))
