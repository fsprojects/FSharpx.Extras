module FSharpx.NonEmptyListTests

open FSharpx
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

type NonEmptyListGen =
    static member NonEmptyList =
        gen.Return NonEmptyList.create 
        |> Gen.ap Arb.generate
        |> Gen.ap Arb.generate
        |> Arb.fromGen

Arb.register<NonEmptyListGen>() |> ignore

let fsCheck t = fsCheck "" t

[<Test>]
let ``toList gives non-empty list``() =
    fsCheck (fun nel -> NonEmptyList.toList nel |> List.length > 0)

[<Test>]
let ``toArray gives non-empty array``() =
    fsCheck (fun nel -> NonEmptyList.toArray nel |> Array.length > 0)

[<Test>]
let ``toList is same length as non-empty list`` () =
    fsCheck (fun nel -> NonEmptyList.toList nel |> List.length = nel.Length)

[<Test>]
let ``toArray is same length as non-empty list`` () =
    fsCheck (fun nel -> NonEmptyList.toArray nel |> Array.length = nel.Length)

[<Test>]
let ``functor laws``() =
    let map = NonEmptyList.map
    fsCheck (fun nel -> map id nel = nel)
    fsCheck (fun f g nel -> map (f << g) nel = (map f << map g) nel)

[<Test>]
let ``reverse . reverse = id`` () =
    fsCheck (fun nel -> (NonEmptyList.rev << NonEmptyList.rev) nel = nel)

[<Test>]
let ``last . reverse = head``() =
    fsCheck (fun nel -> (NonEmptyList.last << NonEmptyList.rev) nel = NonEmptyList.head nel)

[<Test>]
let ``head . reverse = last``() =
    fsCheck (fun nel -> (NonEmptyList.head << NonEmptyList.rev) nel = NonEmptyList.last nel)

[<Test>]
let ``last is last and never fails``() =
    fsCheck <| fun nel ->
        let actualLast = NonEmptyList.last nel
        let expectedLast = 
            let l = NonEmptyList.toList nel
            l.[l.Length-1]
        expectedLast = actualLast

[<Test>]
let ``append has combined length``() =
    fsCheck <| fun a b ->
        let c = NonEmptyList.append a b
        c.Length = a.Length + b.Length

[<Test>]
let reduce() =
    fsCheck <| fun nel ->
        let actual = NonEmptyList.reduce (+) nel
        let expected = nel |> NonEmptyList.toList |> List.sum
        expected = actual

[<Test>]
let ``monad left identity``() =
    fsCheck <| fun f a ->
        let x = NonEmptyList.singleton a |> NonEmptyList.collect f
        x = f a

[<Test>]
let ``monad right identity``() =
    fsCheck <| fun nel ->
        NonEmptyList.collect NonEmptyList.singleton nel = nel

[<Test>]
let ``monad associativity``() =
    let (>>=) m f = NonEmptyList.collect f m
    fsCheck <| fun f g nel ->
        let n1 = (nel >>= f) >>= g
        let n2 = nel >>= (fun x -> f x >>= g)
        n1 = n2