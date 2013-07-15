module FSharpx.Tests.OptionTests

open System
open NUnit.Framework
open FSharpx
open FSharpx.Option

[<Test>]
let ``kleisli composition``() =
    let f x = 
        if x > 5
            then Some "hello"
            else None
    let g x =
        if x = "hello"
            then Some 10
            else None

    let h = f >=> g
    Assert.AreEqual(Some 10, h 8)
    Assert.AreEqual(None, h 1)

[<Test>]
let ``from bool and value``() =
    let parse x = 
        Int32.TryParse(x, System.Globalization.NumberStyles.Integer, System.Globalization.CultureInfo.InvariantCulture) 
        |> Option.ofBoolAndValue
    Assert.AreEqual(Some 34, parse "34")
    Assert.AreEqual(None, parse "xx")

[<Test>]
let ``valid cast``() =
    let a = box 11    
    let r = Option.cast a
    Assert.IsTrue(Option.isSome r)
    Assert.AreEqual(11, Option.get r)

[<Test>]
let ``invalid cast``() =
    let a = box "a string"
    let r : int option = Option.cast a
    Assert.IsTrue(Option.isNone r)

[<Test>]
let ``sequence with Some``() =
    let r = Option.sequence [Some 1; Some 2; Some 3]
    Assert.AreEqual(Some [1;2;3], r)

[<Test>]
let ``sequence with None``() =
    let r = Option.sequence [Some 1; Some 2; None]
    Assert.AreEqual(None, r)

[<Test>]
let ``concat some of some``() =
    let r = Some(Some 1) |> Option.concat
    Assert.AreEqual(Some 1, r)

[<Test>]
let ``concat some of none``() =
    let r = Some(None) |> Option.concat
    Assert.AreEqual(None, r)

[<Test>]
let ``concat none``() =
    let r = None |> Option.concat
    Assert.AreEqual(None, r)

    