module FSharpx.TypeProviders.Tests.PersonTests

open NUnit.Framework
open FsUnit
open FSharpx

type Person = ProtoBuf<"Person.proto">

[<Test>] 
let ``Can create and init properties from person protocol buffer``() =
    let p = Person()

    p.id |> should equal 0
    p.name |> should equal  ""
    p.email |> should equal None

[<Test>] 
let ``Can write and read properties from person protocol buffer``() =
    let p = Person()
    p.id <- 1
    p.name <- "Paula"
    p.email <- Some "paula@gmail.com"

    p.id |> should equal 1
    p.name |> should equal  "Paula"
    p.email |> should equal (Some "paula@gmail.com")

[<Test>] 
let ``Can serialize a person protocol buffer into a stream``() =
    let p = Person()
    p.id <- 1
    p.name <- "Paula"
    p.email <- Some "paula@gmail.com"

    let stream = new System.IO.MemoryStream()
    p.Serialize(stream)

[<Test>] 
let ``Can deserialize a person protocol buffer from a stream``() =
    let stream = new System.IO.MemoryStream()
    let p = Person(stream)

    p.id |> should equal 0
    p.name |> should equal  ""
    p.email |> should equal None
