﻿module FSharpx.Tests.MonoidTests

open NUnit.Framework
open FsCheck
open FsUnitTyped
open FSharpx
open FSharpx.Functional
open FSharpx.Collections
open FSharpx.Tests.Properties
open FSharpx.Monoid

type ByteStringGen =
    static member ByteStringArb() =
        let g = gen {
            let! a = Arb.generate<_[]>
            let! offset = Gen.choose(0, max 0 (a.Length - 1))
            let! count = Gen.choose(0, a.Length - offset)
            return ByteString(a, offset, count)
        }
        Arb.fromGen g

[<OneTimeSetUp>]
let setup() =
    Arb.register<ByteStringGen>() |> ignore

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
    checkMonoid "map" Map.monoid<int,int>

[<Test>]
let ``option monoid``() =
    checkMonoid "option" (Option.monoid (Monoid.product()))

[<Test>]
let ``dual monoid example``() =
    let m = Monoid.dual Monoid.string
    let r1 = m {
        yield "hello"
        yield "world"
    }
    let r2 = m.Combine("hello", "world")
    r1 |> shouldEqual "worldhello"
    r2 |> shouldEqual "worldhello"

[<Test>]
let ``any monoid``() =
    checkMonoid "any" Monoid.any

[<Test>]
let ``all monoid``() =
    checkMonoid "all" Monoid.all

[<Test>]
let ``tuple2 monoid``() =
    checkMonoid "tuple2" (Monoid.tuple2 List.monoid Monoid.all)

[<Test>]
let ``tuple3 monoid``() =
    checkMonoid "tuple3" (Monoid.tuple3 List.monoid Monoid.all Monoid.all)

[<Test>]
let ``min monoid``() =
    checkMonoid "min" Monoid.minInt

[<Test>]
let ``max monoid``() =
    checkMonoid "max" Monoid.maxInt

[<Test>]
let ``bytestring monoid``() =
    checkMonoid "bytestring" ByteString.monoid

[<Test>]
let ``unit monoid``() =
    checkMonoid "unit" Monoid.unit

[<Test>]
let ``endo monoid``() =
    // a -> a does not support equality
    //checkMonoid "endo" Monoid.endo

    // endo composes functions
    // this also shows using a monoid in a computation expression (in this case as a mconcat)
    // also equivalent to Seq.fold (<<) id [(*) 2; (-) 4; (+) 2]
    let composedFunction = 
        Monoid.endo {
            for f in [(*) 2; (+) 4; (+) 2] -> f
        }
    composedFunction 0 |> shouldEqual 12

[<Test>]
let ``endo as dlist``() =
    let (@) a b = Monoid.endo.Combine(a,b)
    let nil = Monoid.endo.Zero()
    let singleton x a = List.monoid.Combine([x], a)
    let toList l = l []
    let z = singleton 4 @ singleton 5
    z |> toList |> shouldEqual [4;5]
