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
    
[<Test>]
let ``Boolean active pattern matches``() =
    match "true" with
    | Boolean x -> Assert.IsTrue x
    | _ -> Assert.Fail "false negative match"
    
    match "4.0" with
    | Boolean x -> Assert.Fail "false positive match"
    | _ -> ()

[<Test>]
let ``Int32 active pattern matches``() =
    match "3" with
    | Int32 x -> Assert.AreEqual(3, x)
    | _ -> Assert.Fail "false negative match"
    
    match "4.0" with
    | Int32 x -> Assert.Fail "false positive match"
    | _ -> ()

[<Test>]
[<SetCulture("de-DE")>]
let ``Double active pattern is culture invariant``() =
    match "3.5" with
    | Double x -> Assert.AreEqual(3.5, x)
    | _ -> Assert.Fail "false negative match"
    
    match "3" with
    | Double x -> Assert.AreEqual(3., x)
    | _ -> Assert.Fail "false negative match"
    
    match "3,5" with
    | Double x -> Assert.Fail "false positive match"
    | _ -> ()