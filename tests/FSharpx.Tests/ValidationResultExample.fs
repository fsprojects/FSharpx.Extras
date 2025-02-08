module FSharpx.Tests.ValidationResultExample

// ported from original in Scalaz: https://gist.github.com/970717
// copy of ValidationExample adjusted for Validation.Result

open FSharpx.Result
open NUnit.Framework
open FsUnitTyped

open FSharpx.Collections
open FSharpx.Validation.Result

// First let's define a domain.

type Sobriety = Sober | Tipsy | Drunk | Paralytic | Unconscious

type Gender = Male | Female

type Person = {
    Gender: Gender
    Age: int
    Clothes: string Set
    Sobriety: Sobriety
}

// Let's define the checks that *all* nightclubs make!
module Club =
    let checkAge (p: Person) =
        if p.Age < 18 then 
            Error "Too young!"
        elif p.Age > 40 then
            Error "Too old!"
        else
            Ok p

    let checkClothes (p: Person) =
        if p.Gender = Male && not (p.Clothes.Contains "Tie") then
            Error "Smarten up!"
        elif p.Gender = Female && p.Clothes.Contains "Trainers" then
            Error "Wear high heels"
        else
            Ok p

    let checkSobriety (p: Person) =
        match p.Sobriety with
        | Drunk | Paralytic | Unconscious -> Error "Sober up!"
        | _ -> Ok p

// Now let's compose some validation checks

module ClubbedToDeath =
    open Club
    // PERFORM THE CHECKS USING Monadic "computation expression" SUGAR
    let either = ResultBuilder()
    let costToEnter p =
        either {
            let! a = checkAge p
            let! b = checkClothes a
            let! c = checkSobriety b
            return 
                match c.Gender with
                | Female -> 0m
                | Male -> 5m
        }

    // or composing functions:

    let costToEnter2 =
        let costByGender (p: Person) = 
            match p.Gender with
            | Female -> 0m
            | Male -> 5m
        let checkAll = checkAge >=> checkClothes >=> checkSobriety // kleisli composition
        checkAll >> Result.map costByGender

// Now let's see these in action

let Ken = { Person.Gender = Male; Age = 28; Clothes = set ["Tie"; "Shirt"]; Sobriety = Tipsy }
let Dave = { Person.Gender = Male; Age = 41; Clothes = set ["Tie"; "Jeans"]; Sobriety = Sober }
let Ruby = { Person.Gender = Female; Age = 25; Clothes = set ["High heels"]; Sobriety = Tipsy }

// let's go clubbing!

[<Test>]
let part1() =
    ClubbedToDeath.costToEnter Dave |> shouldEqual (Error "Too old!")
    ClubbedToDeath.costToEnter Ken |> shouldEqual (Ok 5m)
    ClubbedToDeath.costToEnter Ruby |> shouldEqual (Ok 0m)
    ClubbedToDeath.costToEnter { Ruby with Age = 17 } |> shouldEqual (Error "Too young!")
    ClubbedToDeath.costToEnter { Ken with Sobriety = Unconscious } |> shouldEqual (Error "Sober up!")

(**
 * The thing to note here is how the Validations can be composed together in a computation expression.
 * The type system is making sure that failures flow through your computation in a safe manner.
 *)

(**
 * Part Two : Club Tropicana
 *
 * Part One showed monadic composition, which from the perspective of Validation is *fail-fast*.
 * That is, any failed check shortcircuits subsequent checks. This nicely models nightclubs in the
 * real world, as anyone who has dashed home for a pair of smart shoes and returned, only to be
 * told that your tie does not pass muster, will attest.
 *
 * But what about an ideal nightclub? One that tells you *everything* that is wrong with you.
 *
 * Applicative functors to the rescue!
 *
 *)

module ClubTropicana =
    open Club
    let failToList x = Result.mapError NonEmptyList.singleton x
    let costByGender (p: Person) =
        match p.Gender with
        | Female -> 0m
        | Male -> 7.5m

    //PERFORM THE CHECKS USING applicative functors, accumulating failure via a monoid

    let costToEnter p =
        costByGender <!> (checkAge p |> failToList) *> (checkClothes p |> failToList) *> (checkSobriety p |> failToList)


// And the use? Dave tried the second nightclub after a few more drinks in the pub
[<Test>]
let part2() =
    ClubTropicana.costToEnter { Dave with Sobriety = Paralytic } 
    |> shouldEqual (Error (NonEmptyList.create "Too old!" ["Sober up!"]))

    ClubTropicana.costToEnter Ruby |> shouldEqual (Ok 0m)

(**
 *
 * So, what have we done? Well, with a *tiny change* (and no changes to the individual checks themselves),
 * we have completely changed the behaviour to accumulate all errors, rather than halting at the first sign
 * of trouble. Imagine trying to do this using exceptions, with ten checks.
 *
 *)

(**
 *
 * Part Three : Gay bar
 *
 * And for those wondering how to do this with a *very long list* of checks.
 *
 *)

module GayBar =
    open Club
    let checkGender (p: Person) =
        match p.Gender with
        | Male -> Ok p
        | _ -> Error "Men only"

    let costToEnter p =
        [checkAge; checkClothes; checkSobriety; checkGender]
        |> mapM (fun check -> check p |> Result.mapError NonEmptyList.singleton)
        |> Result.map (function x::_ -> decimal x.Age + 1.5m | [] -> failwith "costToEnter")

[<Test>]
let part3() =
    GayBar.costToEnter { Person.Gender = Male; Age = 59; Clothes = set ["Jeans"]; Sobriety = Paralytic } 
    |> shouldEqual (Error (NonEmptyList.create "Too old!" ["Smarten up!"; "Sober up!"]))

    GayBar.costToEnter { Person.Gender = Male; Age = 25; Clothes = set ["Tie"]; Sobriety = Sober } |> shouldEqual (Ok 26.5m)

