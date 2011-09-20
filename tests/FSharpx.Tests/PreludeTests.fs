module FSharpx.Tests.PreludeTests

open System
open System.Globalization
open System.Threading
open FSharpx
open NUnit.Framework

[<Test>]
[<SetCulture("es-AR")>]
let ``Single parse is culture invariant``() =
    Assert.AreEqual(None, Single.parse "3,5")
    Assert.AreEqual(Some 3.5f, Single.parse "3.5")

[<Test>]
[<SetCulture("es-AR")>]
let ``Double parse is culture invariant``() =
    Assert.AreEqual(None, Double.parse "3,5")
    Assert.AreEqual(Some 3.5, Double.parse "3.5")

[<Test>]
[<SetCulture("es-AR")>]
let ``DateTime parse is culture invariant``() =
    let pt = DateTime.parse "5/6/2011"
    let et = DateTime(2011,5,6)
    Assert.AreEqual(Some et, pt)

[<Test>]
let ``DateTime parseExact``() =
    let pt = "05/06/2011" |> DateTime.parseExact [|"dd/MM/yyyy"|]
    let et = DateTime(2011,6,5)
    Assert.AreEqual(Some et, pt)