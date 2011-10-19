module FSharpx.Tests.EnumeratorTest

open System
open FSharpx
open NUnit.Framework
open FsUnit

module Iteratee =
    open System.Collections.Generic

    type Iteratee<'el, 'a> = Reader.Reader<IEnumerator<'el>, 'a>

    let iteratee = Reader.reader

    let enumerate (i : Iteratee<'el, 'a>) (xs : #seq<'el>) =
        let en = xs.GetEnumerator() in i en

    // Note that Enumerator functions are, more or less, used directly within the Reader monad.
    // This is possible because Reader takes the current environment and returns a response.
    // In the case of iteratee, the Enumerator is the environment. This of course clearly
    // demonstrates the primary difference in this version of iteratee and the Haskell-style iteratee.

    // It should be noted that this head is more like `run`.
    let head en = Enumerator.head en
    let length en = Enumerator.length en
    let skip n en = Enumerator.skip n en |> ignore
    let skipUntil pred en = Enumerator.skipUntil pred en |> ignore
    let skipWhile pred en = Enumerator.skipWhile pred en |> ignore
    let take n en = Enumerator.take n en
    let takeUntil pred en = Enumerator.takeUntil pred en
    let takeWhile pred en = Enumerator.takeWhile pred en
    let map f en = Enumerator.map f en
    let fold f seed en = Enumerator.fold f seed en

    let heads pattern en =
        let rec loop count pattern = Enumerator.iter {
            let! p = pattern
            match p with
            | None -> yield count
            | Some p' ->
                let! e = en
                match e with
                | None -> yield count
                | Some e' when e' <> p' -> yield count
                | Some e' -> yield! loop (count + 1) pattern }
        loop 0 pattern |> Enumerator.head

open Iteratee

[<Test>]
let ``test length``() =
    enumerate length [1..4] |> should equal 4

// Tests skip n and head
[<Test>]
let ``test skip2Head``() =
    let skip2Head = iteratee {
        do! skip 2
        return! head }
    enumerate skip2Head [1..4] |> should equal 3

[<Test>]
let ``test skip2Head2``() =
    let skip2Head2 = iteratee {
        do! skip 2
        let! _ = head
        return! head }
    enumerate skip2Head2 [1..4] |> should equal 4

[<Test>]
let ``test skipUntil``() =
    let skipUntil3Head = iteratee {
        do! skipUntil ((=) 3)
        return! head }
    enumerate skipUntil3Head [1..4] |> should equal 3

[<Test>]
let ``test skipWhile``() =
    let skipWhileLessThan3Head = iteratee {
        do! skipWhile ((>) 3)
        return! head }
    enumerate skipWhileLessThan3Head [1..4] |> should equal 3

[<Test>]
let ``test take 2``() =
    let actual = enumerate (take 2) [1..4]
    (fun () -> actual) |> Enumerator.toSeq |> List.ofSeq |> should equal [1;2]

[<Test>]
let ``test takeUntil``() =
    let actual = enumerate (takeUntil <| (=) 3) [1..4]
    (fun () -> actual) |> Enumerator.toSeq |> List.ofSeq |> should equal [1;2]

[<Test>]
let ``test takeWhile``() =
    let actual = enumerate (takeWhile <| (>) 3) [1..4]
    (fun () -> actual) |> Enumerator.toSeq |> List.ofSeq |> should equal [1;2]

[<Test>]
let ``test map``() =
    let actual = enumerate (map <| fun i -> i.ToString()) [1..4]
    (fun () -> actual) |> Enumerator.toSeq |> List.ofSeq |> should equal ["1";"2";"3";"4"]

// Tests scan and last as well as fold.
[<Test>]
let ``test fold``() =
    let sum = fold (+) 0
    enumerate sum [1..4] |> should equal 10

[<Test>]
let ``test heads``() =
    // Two options for creating the pattern:
    //    let pattern = Enumerator.iter { yield 'a'; yield 'b'; yield 'c' }
    let pattern = ("abc").GetEnumerator()
    enumerate (heads pattern) "abd" |> should equal 2
