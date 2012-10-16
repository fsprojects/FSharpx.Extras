module FSharpx.Tests.Properties

open FsCheck
open FsCheck.NUnit

let checkEquality<'a when 'a : equality> name =
    let n = sprintf "%s : equality %s" name
    fsCheck (n "identity") <| 
        fun (x: 'a) -> x = x
    fsCheck (n "predicate") <|
        // depends a lot on the quality of the generated predicates, i.e. a constant predicate isn't useful.
        fun (f: 'a -> bool) x y -> f x = f y
    fsCheck (n "transitive") <|
        // not very useful, it's not likely that relevant values will be generated
        fun (x: 'a) (y: 'a) (z: 'a) ->
            if x = y && y = z then x = z else true

module Gen =
    let rec infiniteSeq() =
        gen {
            let! x = Arb.generate
            let! xs = infiniteSeq()
            return Seq.append (Seq.singleton x) xs
        }

    let infiniteLazyList() =
        Gen.map LazyList.ofSeq (infiniteSeq())

    let finiteLazyList() =
        Gen.map LazyList.ofList Arb.generate