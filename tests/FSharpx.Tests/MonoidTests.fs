module FSharpx.Tests.MonoidTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FSharpx
open FSharpx.Tests.Properties
open FSharpx.Monoid

[<Test>]
let ``int product monoid``() =
    checkMonoid "int product" IntProductMonoid

[<Test>]
let ``int sum monoid``() =
    checkMonoid "int sum" IntSumMonoid

[<Test>]
let ``list monoid``() =
    checkMonoid "list" (ListMonoid<obj>())

[<Test>]
let ``string monoid``() =
    checkMonoid "string" StringMonoid

[<Test>]
let ``option monoid``() =
    checkMonoid "option" (OptionMonoid IntProductMonoid)

[<Test>]
let ``dual monoid``() =
    checkMonoid "dual" (DualMonoid StringMonoid)

[<Test>]
let ``dual monoid example``() =
    let m = DualMonoid StringMonoid
    let r1 = m {
        yield "hello"
        yield "world"
    }
    let r2 = m.Combine("hello", "world")
    Assert.AreEqual("worldhello", r1)
    Assert.AreEqual("worldhello", r2)

[<Test>]
let ``any monoid``() =
    checkMonoid "any" AnyMonoid

[<Test>]
let ``all monoid``() =
    checkMonoid "all" AllMonoid

[<Test>]
let ``tuple2 monoid``() =
    checkMonoid "tuple2" (Tuple2Monoid(ListMonoid<int>(), StringMonoid))

[<Test>]
let ``tuple3 monoid``() =
    checkMonoid "tuple3" (Tuple3Monoid(ListMonoid<int>(), StringMonoid, AllMonoid))