module FSharpx.Tests.Properties

open FSharpx
open FSharpx.Collections
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

let checkMonoid name (monoid: _ FSharpx.Monoid) =
    let n = sprintf "%s : monoid %s" name
    let mappend = curry monoid.Combine
    let mempty = monoid.Zero()
    fsCheck (n "left identity") <|
        fun a -> mappend a mempty = a
    fsCheck (n "right identity") <|
        fun a -> mappend mempty a = a
    fsCheck (n "associativity") <|
        fun x y z -> mappend (mappend x y) z = mappend x (mappend y z)

let classifyCollect xs (count : int) (y : bool) =
    y |> Prop.collect count
    |> Prop.classify (xs.GetType().FullName.Contains("System.Int32")) "int"  
    |> Prop.classify (xs.GetType().FullName.Contains("System.String")) "string"
    |> Prop.classify (xs.GetType().FullName.Contains("System.Boolean")) "bool"
    |> Prop.classify (xs.GetType().FullName.Contains("System.Object")) "object"

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

(*
Recommend a range of size 1 - 12 for lists used to build test data structures:

Several data structures, especially those where the internal data representation is either binary or skew binary, have "distinct failure modes
across the low range of sizes". What I mean by this is "it is possible to have bugs specific to certain small sizes in these data structures". So it is 
important that every structure size up to a certain value (let us say "8" for arguments sake) needs to be tested every time. By default FsCheck generates 
100 (pseudo-random) lists. The larger the size range you allow for generation, the higher the chance these crucial small sizes will be skipped.
*)
    let listBool n  = Gen.listOfLength n Arb.generate<bool>

    let listInt n  = Gen.listOfLength n Arb.generate<int>

    let listObj n  = Gen.listOfLength n Arb.generate<obj>

    let listString n  = Gen.listOfLength n Arb.generate<string>

    let length1thru12 = Gen.choose (1, 12)

    let length2thru12 = Gen.choose (2, 12)