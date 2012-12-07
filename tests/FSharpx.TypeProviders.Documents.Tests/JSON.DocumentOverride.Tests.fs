module FSharpx.TypeProviders.Tests.JSON.DocumentOverride.Tests

open NUnit.Framework
open FSharpx
open System.Xml
open FsUnit

type WikiSample =
    JsonZipper<Schema=
        """{  
                 "firstName": "John",
                 "lastName" : "Smith",
                 "age"      : 25
           }""">

let newJson = 
    """{  
            "firstName": "Jane",
            "lastName" : "Doe",
            "age"      : 23
    }"""

let newJson2 = 
    """{  
            "firstName": "Jim",
            "lastName" : "Smith",
            "age"      : 24
    }"""

let document = WikiSample(documentContent=newJson)
let document2 = WikiSample(documentContent=newJson2)

[<Test>]
let ``Jane should have first name of Jane``() = 
    document.FirstName.GetValue() |> should equal "Jane"

[<Test>]
let ``Jane should have a last name of Doe``() = 
    document.LastName.GetValue() |> should equal "Doe"

[<Test>]
let ``Jane should have an age of 23``() = 
    document.Age.GetValue() |> should equal 23

[<Test>]
let ``Jim should have a first name of Jim``() = 
    document2.FirstName.GetValue() |> should equal "Jim"

[<Test>]
let ``Jim should have a last name of Smith``() = 
    document2.LastName.GetValue() |> should equal "Smith"

[<Test>]
let ``Jim should have an age of 24``() = 
    document2.Age.GetValue() |> should equal 24
