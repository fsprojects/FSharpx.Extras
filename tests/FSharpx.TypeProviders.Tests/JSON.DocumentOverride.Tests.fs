module FSharpx.TypeProviders.Tests.JSON.DocumentOverride.Tests

open NUnit.Framework
open FSharpx
open FsUnit
open System.Xml

type WikiSample =
    StructuredJSON<Schema=
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

let document = WikiSample(documentContent=newJson).Root
let document2 = WikiSample(documentContent=newJson2).Root

[<Test>]
let ``Jane should have first name of Jane``() = 
    document.FirstName |> should equal "Jane"

[<Test>]
let ``Jane should have a last name of Doe``() = 
    document.LastName |> should equal "Doe"

[<Test>]
let ``Jane should have an age of 23``() = 
    document.Age |> should equal 23

[<Test>]
let ``Jim should have a first name of Jim``() = 
    document2.FirstName |> should equal "Jim"

[<Test>]
let ``Jim should have a last name of Smith``() = 
    document2.LastName |> should equal "Smith"

[<Test>]
let ``Jim should have an age of 24``() = 
    document2.Age |> should equal 24
