module FSharpx.TypeProviders.Tests.JsonZipper.Obj.ZipperTests

open NUnit.Framework
open FSharpx
open FsUnit

type Simple = JsonZipper<Schema="""{ "a": "b"}""">

[<Test>]
let ``Can create a zipper from inlined JSON``() = 
    let original = new Simple()
    original.ToString() |> should equal """{"a":"b"}"""

[<Test>]
let ``Can read a text property in a simple JSON``() = 
    let original = new Simple()
    original.A.GetValue() |> should equal "b"
    original.ToString() |> should equal """{"a":"b"}"""

[<Test>]
let ``Can update a text property in a simple JSON without changing the original``() = 
    let original = new Simple()
    let updated = original.A.Update("c")
    updated.ToString() |> should equal """{"a":"c"}"""
    original.ToString() |> should equal """{"a":"b"}"""

type Simple1 = JsonZipper<Schema="""{ "b": 1}""">

[<Test>]
let ``Can read an int property``() = 
    let original = new Simple1()
    original.B.GetValue() |> should equal 1
    original.ToString() |> should equal """{"b":1}"""

[<Test>]
let ``Can update an int property in a simple JSON``() = 
    let original = new Simple1()
    let updated = original.B.Update(2)
    updated.ToString() |> should equal """{"b":2}"""
    original.ToString() |> should equal """{"b":1}"""

type Simple2 = JsonZipper<Schema="""{ "a": "b", "b": 1}""">

[<Test>]
let ``Can update two properties in a simple JSON``() = 
    let original = new Simple2()
    let updated = original.B.Update(2).A.Update("blub")
    updated.ToString() |> should equal """{"a":"blub","b":2}"""
    original.ToString() |> should equal """{"a":"b","b":1}"""
    
type Simple3 = JsonZipper<Schema="""{ "b": "blub", "a": 1}""">

[<Test>]
let ``Update preserves the order in a JSON document``() = 
    let original = new Simple3()
    let updated = original.B.Update("bla").A.Update(3)
    updated.ToString() |> should equal """{"b":"bla","a":3}"""
    original.ToString() |> should equal """{"b":"blub","a":1}"""
    
type Nested = JsonZipper<Schema="""{ "a": "b", "b": { "c": "text" }}""">

[<Test>]
let ``Can update a property in a nested JSON``() = 
    let original = new Nested()
    let updated = original.B.C.Update("blub")
    updated.Top().ToString() |> should equal """{"a":"b","b":{"c":"blub"}}"""
    original.ToString() |> should equal """{"a":"b","b":{"c":"text"}}"""

[<Test>]
let ``Can update a property in a nested JSON multiple times``() = 
    let original = new Nested()
    let updated = original.B.C.Update("blub").C.Update("foo").C.Update("bar")
    updated.Top().ToString() |> should equal """{"a":"b","b":{"c":"bar"}}"""
    original.ToString() |> should equal """{"a":"b","b":{"c":"text"}}"""
     
type Nested2 = JsonZipper<Schema="""{ "a": "b", "b": { "c": 1.3,"d" : true }}""">

[<Test>]
let ``Can access a decimal property in a nested JSON``() = 
    let original = new Nested2()
    let updated = original.B.C.Update 4.55m
    updated.C.GetValue() |> should equal 4.55m
    original.B.C.GetValue() |> should equal 1.3m
    updated.Top().ToString() |> should equal """{"a":"b","b":{"c":4.55,"d":true}}"""
    
    original.ToString() |> should equal """{"a":"b","b":{"c":1.3,"d":true}}"""

[<Test>]
let ``Can access a boolean property in a nested JSON``() = 
    let original = new Nested2()
    let updated = original.B.D.Update false
    updated.Top().ToString() |> should equal """{"a":"b","b":{"c":1.3,"d":false}}"""
    original.ToString() |> should equal """{"a":"b","b":{"c":1.3,"d":true}}"""


type DoubleNested = JsonZipper<Schema="""{ "a": "b", "b": { "c": { "d" : "down here" } }}""">

[<Test>]
let ``Can update a property in a deeply nested JSON``() = 
    let original = new DoubleNested()
    let updated = original.B.C.D.Update("blub")
    updated.Top().ToString() |> should equal """{"a":"b","b":{"c":{"d":"blub"}}}"""
    original.ToString() |> should equal """{"a":"b","b":{"c":{"d":"down here"}}}"""


[<Test>]
let ``Can access top in a nested JSON``() = 
    let original = new DoubleNested()
    original.B.Top().ToString() |> should equal """{"a":"b","b":{"c":{"d":"down here"}}}"""
    original.B.C.Top().ToString() |> should equal """{"a":"b","b":{"c":{"d":"down here"}}}"""
    original.ToString() |> should equal """{"a":"b","b":{"c":{"d":"down here"}}}"""


[<Test>]
let ``Can access up in a nested JSON``() = 
    let original = new DoubleNested()
    original.B.Up().ToString() |> should equal """{"a":"b","b":{"c":{"d":"down here"}}}"""
    original.B.Up().B.C.Up().C.Top().ToString() |> should equal """{"a":"b","b":{"c":{"d":"down here"}}}"""