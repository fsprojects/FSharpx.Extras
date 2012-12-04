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
    let a = emptyObject |> addStringProperty "firstName" "John"
    let b = emptyObject |> addStringProperty "firstName" "John"
    let c = emptyObject |> addStringProperty "firstName" "Fred"
    let d = emptyObject |> addStringProperty "lastName" "Fred"

    Assert.AreEqual(a,b)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(c,d)

[<Test>]
let ``Can compare documents with multiple properties``() =
    let a = emptyObject |> addBoolProperty "aa" true |> addDecimalProperty "bb" (decimal 42.42)
    let b = emptyObject |> addBoolProperty "aa" true |> addDecimalProperty "bb" (decimal 42.42)
    let c = emptyObject |> addBoolProperty "aa" true |> addDecimalProperty "bb" (decimal 42.43)
    let d = emptyObject |> addBoolProperty "aa" true |> addDecimalProperty "bb" (decimal 42.42) |> addDecimalProperty "c" (decimal 42.42)
      
    Assert.AreEqual(a,b)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(a,d)

[<Test>]
let ``Can compare documents with JObjects``() =
    let a = emptyObject |> addProperty "sub" (emptyObject |> addStringProperty "t" "test")
    let b = emptyObject |> addProperty "sub" (emptyObject |> addStringProperty "t" "test")
    let c = emptyObject |> addProperty "sub1" (emptyObject |> addStringProperty "t" "test")
    let d = emptyObject |> addProperty "sub" (emptyObject |> addStringProperty "t" "test1")
      
    Assert.AreEqual(a,b)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(a,d)

[<Test>]
let ``Can compare documents with JArray``() =
    let a = emptyArray |> addElement (emptyObject |> addStringProperty "t" "test")
    let b = emptyArray |> addElement (emptyObject |> addStringProperty "t" "test")
    let c = emptyArray |> addElement (emptyObject |> addStringProperty "t" "test1")
    let d = emptyArray |> addElement (emptyObject |> addStringProperty "t" "test") |> addElement (emptyObject |> addStringProperty "t1" "test1")
    let e = emptyArray |> addElement (emptyObject |> addStringProperty "t" "test") |> addElement (emptyObject |> addStringProperty "t1" "test1")
      
    Assert.AreEqual(a,b)
    Assert.AreEqual(d,e)
    Assert.AreNotEqual(a,c)
    Assert.AreNotEqual(a,d)