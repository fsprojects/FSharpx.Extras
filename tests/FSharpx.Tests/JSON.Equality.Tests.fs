module FSharpx.Tests.JSON.EqualityTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit

[<Test>]
let ``Can compare empty documents``() = 
    JsonValue.Obj(Map.empty) |> should equal (JsonValue.Obj(Map.empty))
    JsonValue.Obj(Map.empty) |> should equal emptyObject

[<Test>] 
let ``Can compare documents with single property``() =
    let a = emptyObject.AddStringProperty("firstName","John")
    let b = emptyObject.AddStringProperty("firstName","John")
    let c = emptyObject.AddStringProperty("firstName","Fred")
    let d = emptyObject.AddStringProperty("lastName","Fred")

    Assert.AreEqual(a,b)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(c,d)

[<Test>]
let ``Can compare documents with multiple properties``() =
    let a = emptyObject.AddBoolProperty("aa",true).AddDecimalProperty("bb",decimal 42.42)
    let b = emptyObject.AddBoolProperty("aa",true).AddDecimalProperty("bb",decimal 42.42)
    let c = emptyObject.AddBoolProperty("aa",true).AddDecimalProperty("bb",decimal 42.43)
    let d = emptyObject.AddBoolProperty("aa",true).AddDecimalProperty("bb",decimal 42.42).AddDecimalProperty("c",decimal 42.42)
      
    Assert.AreEqual(a,b)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(a,d)

[<Test>]
let ``Can compare documents with JObjects``() =
    let a = emptyObject.AddProperty("sub",emptyObject.AddStringProperty("t","test"))
    let b = emptyObject.AddProperty("sub",emptyObject.AddStringProperty("t","test"))
    let c = emptyObject.AddProperty("sub1",emptyObject.AddStringProperty("t","test"))
    let d = emptyObject.AddProperty("sub",emptyObject.AddStringProperty("t","test1"))
      
    Assert.AreEqual(a,b)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(a,d)

[<Test>]
let ``Can compare documents with JArray``() =
    let a = emptyArray.AddElement(emptyObject.AddStringProperty("t","test"))
    let b = emptyArray.AddElement(emptyObject.AddStringProperty("t","test"))
    let c = emptyArray.AddElement(emptyObject.AddStringProperty("t","test1"))
    let d = emptyArray.AddElement(emptyObject.AddStringProperty("t","test")).AddElement(emptyObject.AddStringProperty("t1","test1"))
    let e = emptyArray.AddElement(emptyObject.AddStringProperty("t","test")).AddElement(emptyObject.AddStringProperty("t1","test1"))
      
    Assert.AreEqual(a,b)
    Assert.AreEqual(d,e)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(a,d)