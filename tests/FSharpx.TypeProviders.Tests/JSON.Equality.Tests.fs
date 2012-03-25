module FSharp.TypeProviders.Tests.JSON.EqualityTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit

[<Test>]
let ``Can compare empty documents``() = 
    JObject.New() |> should equal (JObject.New())

[<Test>] 
let ``Can compare documents with single property``() =
    let a = JObject.New().AddTextProperty("firstName","John")
    let b = JObject.New().AddTextProperty("firstName","John")
    let c = JObject.New().AddTextProperty("firstName","Fred")
    let d = JObject.New().AddTextProperty("lastName","Fred")

    Assert.AreEqual(a,b)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(c,d)

[<Test>]
let ``Can compare documents with multiple properties``() =
    let a = JObject.New().AddBoolProperty("aa",true).AddNumberProperty("bb",42.42)
    let b = JObject.New().AddBoolProperty("aa",true).AddNumberProperty("bb",42.42)
    let c = JObject.New().AddBoolProperty("aa",true).AddNumberProperty("bb",42.43)
    let d = JObject.New().AddBoolProperty("aa",true).AddNumberProperty("bb",42.42).AddNumberProperty("c",42.42)
      
    Assert.AreEqual(a,b)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(a,d)

[<Test>]
let ``Can compare documents with JObjects``() =
    let a = JObject.New().AddProperty("sub",JObject.New().AddTextProperty("t","test"))
    let b = JObject.New().AddProperty("sub",JObject.New().AddTextProperty("t","test"))
    let c = JObject.New().AddProperty("sub1",JObject.New().AddTextProperty("t","test"))
    let d = JObject.New().AddProperty("sub",JObject.New().AddTextProperty("t","test1"))
      
    Assert.AreEqual(a,b)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(a,d)

[<Test>]
let ``Can compare documents with JArray``() =
    let a = JArray.New().AddElement(JObject.New().AddTextProperty("t","test"))
    let b = JArray.New().AddElement(JObject.New().AddTextProperty("t","test"))
    let c = JArray.New().AddElement(JObject.New().AddTextProperty("t","test1"))
    let d = JArray.New().AddElement(JObject.New().AddTextProperty("t","test"))
                .AddElement(JObject.New().AddTextProperty("t1","test1"))
    let e = JArray.New().AddElement(JObject.New().AddTextProperty("t","test"))
                .AddElement(JObject.New().AddTextProperty("t1","test1"))
      
    Assert.AreEqual(a,b)
    Assert.AreEqual(d,e)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(a,d)