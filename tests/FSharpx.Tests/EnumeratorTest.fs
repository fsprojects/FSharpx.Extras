module FSharpx.Tests.EnumeratorTest

open FSharpx
open NUnit.Framework
open FsUnit

[<Test>]
let ``test skip2Head``() =
    let input = seq { 1..4 }
    let input' = input.GetEnumerator()
    // Reader + Enumerator = Iteratee
    let skip2Head = Reader.reader {
        do! Enumerator.skip 2
        return! Enumerator.head }
    skip2Head input' |> should equal 3

[<Test>]
let ``test skip2Head2``() =
    let input = seq { 1..4 }
    let input' = input.GetEnumerator()
    // Reader + Enumerator = Iteratee
    let skip2Head = Reader.reader {
        do! Enumerator.skip 2
        let! _ = Enumerator.head
        return!  Enumerator.head }
    skip2Head input' |> should equal 4
