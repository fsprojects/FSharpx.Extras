module FSharpx.Tests.MonoidTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
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