module FSharpx.Tests.MonoidTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FSharpx
open FSharpx.Tests.Properties
open FSharpx.Monoid

[<Test>]
let ``int product monoid``() =
    checkMonoid "int product" Monoid.intProduct

[<Test>]
let ``int sum monoid``() =
    checkMonoid "int sum" Monoid.intSum

[<Test>]
let ``list monoid``() =
    checkMonoid "list" (List.monoid)

[<Test>]
let ``set monoid``() =
    checkMonoid "set" (Set.monoid<int>)

[<Test>]
let ``map monoid``() =
    checkMonoid "" (Map.monoid<int,int>)

[<Test>]
let ``string monoid``() =
    checkMonoid "string" Monoid.string

[<Test>]
let ``option monoid``() =
    checkMonoid "option" (Option.monoid Monoid.intProduct)

[<Test>]
let ``dual monoid``() =
    checkMonoid "dual" (Monoid.dual Monoid.string)

[<Test>]
let ``dual monoid example``() =
    let m = Monoid.dual Monoid.string
    let r1 = m {
        yield "hello"
        yield "world"
    }
    let r2 = m.Combine("hello", "world")
    Assert.AreEqual("worldhello", r1)
    Assert.AreEqual("worldhello", r2)

[<Test>]
let ``any monoid``() =
    checkMonoid "any" Monoid.any

[<Test>]
let ``all monoid``() =
    checkMonoid "all" Monoid.all

[<Test>]
let ``tuple2 monoid``() =
    checkMonoid "tuple2" (Monoid.tuple2 List.monoid Monoid.string)

[<Test>]
let ``tuple3 monoid``() =
    checkMonoid "tuple3" (Monoid.tuple3 List.monoid Monoid.string Monoid.all)

