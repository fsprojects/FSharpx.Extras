module FSharpx.Tests.MonoidTests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FSharpx
open FSharpx.Tests.Properties
open FSharpx.Monoid

type ByteStringGen =
    static member ByteStringArb =
        let g = gen {
            let! a = Arb.generate<_[]>
            let! offset = Gen.choose(0, max 0 (a.Length - 1))
            let! count = Gen.choose(0, a.Length - offset)
            return ByteString(a, offset, count)
        }
        Arb.fromGen g

let bytestringArbRegister = lazy (FsCheck.Arb.register<ByteStringGen>() |> ignore)

[<Test>]
let ``int product monoid``() =
    checkMonoid "int product" Monoid.productInt

[<Test>]
let ``int sum monoid``() =
    checkMonoid "int sum" Monoid.sumInt

[<Test>]
let ``list monoid``() =
    checkMonoid "list" List.monoid

[<Test>]
let ``set monoid``() =
    checkMonoid "set" Set.monoid<int>

[<Test>]
let ``map monoid``() =
    checkMonoid "" Map.monoid<int,int>

[<Test>]
let ``string monoid``() =
    checkMonoid "string" Monoid.string

[<Test>]
let ``option monoid``() =
    checkMonoid "option" (Option.monoid (Monoid.product()))

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

[<Test>]
let ``min monoid``() =
    checkMonoid "min" Monoid.minInt

[<Test>]
let ``max monoid``() =
    checkMonoid "max" Monoid.maxInt

[<Test>]
let ``bytestring monoid``() =
    bytestringArbRegister.Force()
    checkMonoid "bytestring" ByteString.monoid