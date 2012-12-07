module FSharpx.TypeProviders.Tests.JSON.WikiSampleTests

open NUnit.Framework
open FSharpx
open FsUnit

type WikiSample =
    JsonZipper<Schema=
        """{  
                 "firstName": "John",
                 "lastName" : "Smith",
                 "age"      : 25,
                 "address"  :
                 {
                     "streetAddress": "21 2nd Street",
                     "city"         : "New York",
                     "state"        : "NY",
                     "postalCode"   : "10021"
                 },
                 "phoneNumber":
                 [
                     {
                       "type"  : "home",
                       "number": "212 555-1234"
                     },
                     {
                       "type"  : "fax",
                       "number": "646 555-4567"
                     }
                 ]
             }""">

[<Test>]
let ``Can parse wiki sample``() = 
    let document = WikiSample()
    document.FirstName.GetValue() |> should equal "John"

    let phones = document.PhoneNumbers
    phones.GetCount() |> should equal 2

    let home = phones.GetElement(0)
    home.Number.GetValue() |> should equal "212 555-1234"
    home.Type.GetValue() |> should equal "home"

    let fax = phones.GetElement(1)
    fax.Number.GetValue() |> should equal "646 555-4567"
    fax.Type.GetValue() |> should equal "fax"

[<Test>]
let ``Can load and manipulate wiki data``() = 
    let document = WikiSample(filename="WikiData.json")
    document.FirstName.GetValue() |> should equal "John"
    document.LastName.GetValue() |> should equal "Doe"

    let phone = document.PhoneNumbers
    phone.GetCount() |> should equal 0

//    document.NewPhoneNumber(Type="home",Number="456 123-4567")
//    |> document.AddPhoneNumber
//
//    document.GetPhoneNumbers() |> Seq.length |> should equal 1

[<Test>]
let ``Can load empty json file and fails on property access``() = 
    let document = WikiSample(filename="Empty.json")
    let failed = ref false
    try
        document.FirstName.GetValue() |> ignore
    with
    | _ -> failed := true
    Assert.IsTrue !failed