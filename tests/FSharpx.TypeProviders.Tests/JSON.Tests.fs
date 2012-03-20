module FSharp.TypeProviders.Tests.JSONParserTests

open NUnit.Framework
open FSharpx
open FsUnit
//
//type S = JSON< "\"John\"">
//let s = S()
//
//[<Test>] 
//let ``Can parse string``() = 
//    s.Text
//    |> should equal "John"
//
//type B = JSON< "true">
//let b = B()
//
//[<Test>] 
//let ``Can parse boolean``() = 
//    b.Boolean
//    |> should equal true
//
//
//type SingleProperty = JSON< "{\"firstName\":\"John\"}">
//let sp = SingleProperty()
//
//[<Test>] 
//let ``Can parse single property``() = 
//    sp.firstName
//    |> should equal "John"
//
//type MultiProperty = JSON< "{\"firstName\":\"John\",\"lastName\":\"Smith\",\"age\":25}">
//let mp = MultiProperty()
//
//[<Test>] 
//let ``Can parse multi property``() = 
//    mp.firstName
//    |> should equal "John"
//
//    mp.age
//    |> should equal 25
//
//type Nested = JSON< "{\"main\":{\"title\":\"example\"}}">
//
//let n = Nested()
//
//[<Test>] 
//let ``Can parse nested json``() = 
//    n.main.title
//    |> should equal "example"
//
//
//type DoubleNested = JSON< "{\"main\":{\"title\":\"example\",\"nested\":{\"nestedTitle\":\"sub\"}}}">
//
//let dn = DoubleNested()
//
//[<Test>] 
//let ``Can parse double nested json``() = 
//    dn.main.nested.nestedTitle
//    |> should equal "sub"
//
//type SimpleArray = JSON< "{\"items\":[{\"id\":\"Open\"},{\"id\":\"Pause\"}]}">
//
//let sa = SimpleArray()
//
//[<Test>] 
//let ``Can parse simple arrays``() = 
//    let list = sa.items
//
////    list
////    |> List.length
////    |> should equal 2
//
//    list.[0].id
//    |> should equal "Open"
//
//    list.[1].id
//    |> should equal "Pause"