module FSharpx.Tests.ResultTests

open System
open NUnit.Framework
open FSharpx
open FSharpx.Result
open FsUnitTyped
open TestHelpers

let areEqual (expected, actual) =
    if expected<>actual then Assert.Fail( sprintf "Expected %A to equal %A" actual expected)

[<Test>]
let ``kleisli composition``() =
    let f x =
        if x > 5
            then Ok "hello"
            else Error ()
    let g x =
        if x = "hello"
            then Ok 10
            else Error ()

    let h = f >=> g
    areEqual(Ok 10, h 8)
    areEqual(Error (), h 1)


[<Test>]
let ``valid cast``() =
    let a = box 11
    let r = Result.cast a
    Assert.AreEqual(11, Result.get r)

[<Test>]
let ``invalid cast``() =
    let a = box "a string"
    let r : Result<int,_> = Result.cast a
    match r with
    | Error _ -> ()
    | Ok _ -> Assert.Fail "Expected cast to fail"

[<Test>]
let ``sequence with Ok``() =
    let r = Result.sequence [Ok 1; Ok 2; Ok 3]
    areEqual(Ok [1;2;3], r)

[<Test>]
let ``sequence with Error``() =
    let r = Result.sequence [Ok 1; Ok 2; Error ()]
    areEqual(Error(), r)

type EmailValidation =
  | EmptyEmail
  | NoAt

let failIfEmpty email =
  if String.IsNullOrEmpty(email) then Error EmptyEmail
  else Ok email

let failIfNotAt (email : string) =
  if (email.Contains("@")) then Ok email
  else Error NoAt

let validateEmail = failIfEmpty >> Result.bind failIfNotAt

let testValidateEmail email (expected : Result<string, EmailValidation>) =
  let actual = validateEmail email
  areEqual(expected, actual)

[<Test>]
let ``Can chain together successive validations``() =
  testValidateEmail "" (Error EmptyEmail)
  testValidateEmail "something_else" (Error NoAt)
  testValidateEmail "some@email.com" (Ok "some@email.com")

// ### Tests ported from Chessie

let shouldBeOkWithValue (expected:'a) (maybeOk:Result<'a,'e>) =
   match maybeOk with
   | Error e-> Assert.Fail( sprintf "Expected Ok, but got Error %A" e)
   | Ok v-> Assert.AreEqual(expected, v)

let shouldBeErrorWithValue (expected:'e) (maybeError:Result<'a,'e>) =
   match maybeError with
   | Error e-> Assert.AreEqual(expected, e)
   | Ok v-> Assert.Fail( sprintf "Expected Error, but got Ok %A" v)

type Sobriety =
  | Sober
  | Tipsy
  | Drunk
  | Paralytic
  | Unconscious

type Gender =
  | Male
  | Female

type Person =
  { Gender : Gender
    Age : int
    Clothes : string Set
    Sobriety : Sobriety }

// Let's define the checks that *all* nightclubs make!
module Club =
  let checkAge (p : Person) =
    if p.Age < 18 then Error "Too young!"
    elif p.Age > 40 then Error "Too old!"
    else Ok()

  let checkClothes (p : Person) =
    if p.Gender = Male && not (p.Clothes.Contains "Tie") then Error "Smarten up!"
    elif p.Gender = Female && p.Clothes.Contains "Trainers" then Error "Wear high heels"
    else Ok()

  let checkSobriety (p : Person) =
    match p.Sobriety with
    | Drunk | Paralytic | Unconscious -> Error "Sober up!"
    | _ -> Ok()

open Club

let costToEnter p =
  result {
    do! checkAge p
    do! checkClothes p
    do! checkSobriety p
    return match p.Gender with
           | Female -> 0m
           | Male -> 5m
  }


let Ken =
  { Person.Gender = Male
    Age = 28
    Clothes = set [ "Tie"; "Shirt" ]
    Sobriety = Tipsy }

let Dave =
  { Person.Gender = Male
    Age = 41
    Clothes = set [ "Tie"; "Jeans" ]
    Sobriety = Sober }

let Ruby =
  { Person.Gender = Female
    Age = 25
    Clothes = set [ "High heels" ]
    Sobriety = Tipsy }

[<Test>]
let part1() =
  costToEnter Dave |> shouldBeErrorWithValue "Too old!"
  costToEnter Ken |> shouldBeOkWithValue 5m
  costToEnter Ruby |> shouldBeOkWithValue 0m
  costToEnter { Ruby with Age = 17 } |> shouldBeErrorWithValue "Too young!"
  costToEnter { Ken with Sobriety = Unconscious } |> shouldBeErrorWithValue "Sober up!"

[<Test>]
let ``Using CE syntax should be equivilent to bind``() =
  let sut =
    result {
      let! bob = Ok "bob"
      let greeting = sprintf "Hello %s" bob
      return greeting
    }
  sut |> shouldBeOkWithValue (sprintf "Hello %s" "bob")

[<Test>]
let ``Try .. with works in CE syntax``() =
  let sut =
    result {
      return! try
                failwith "bang"
                Error("not bang")
              with e -> Ok(e.Message)
    }
  sut |> shouldBeOkWithValue "bang"

let errorIfFalse v =
  if v then Ok()
  else Error()

let func param sideEffect =
  result {
    do! errorIfFalse param
    sideEffect()
    return param
  }

[<Test>]
let ``SideEffects 1: Should return correct value of happy path``() =
  let mutable count = 0
  let sideEffect() = count <- count + 1
  let res = func true sideEffect
  areEqual(Ok true, res)
  areEqual(1, count)

[<Test>]
let ``SideEffects 1: Should return correct value of failing path``() =
  let mutable count = 0
  let sideEffect() = count <- count + 1
  let res = func false sideEffect
  areEqual(Error(), res)
  areEqual(0, count)

let funcDo param sideEffect =
  result {
    do! errorIfFalse param
    do! sideEffect()
    return param
  }

[<Test>]
let ``SideEffects 2 do: Should return correct value of happy path``() =
  let mutable count = 0

  let sideEffect() =
    count <- count + 1
    Ok()

  let res = funcDo true sideEffect
  areEqual(Ok true, res)
  areEqual(1, count)

[<Test>]
let ``SideEffects 2 do: Should return correct value of failing path``() =
  let mutable count = 0

  let sideEffect() =
    count <- count + 1
    Ok()

  let res = funcDo false sideEffect
  areEqual(Error(), res)
  areEqual(0, count)

[<Test>]
let ``use should dispose underlying IDisposable on Ok``() =
  let disposeChecker = new DisposeChecker()
  let r =
     result{
       use! x = Ok disposeChecker
       return x.Disposed
     }
  Assert.Multiple
    (fun () ->
      disposeChecker.Disposed |> shouldEqual true
      r |> shouldEqual (Ok false)
    )

[<Test>]
let ``use should dispose underlying IDisposable on Error``() =
  let disposeChecker = new DisposeChecker()
  let r =
     result{
       use! x = Ok disposeChecker
       let! y = Error "error"
       return x.Disposed
     }
  Assert.Multiple
    (fun () ->
      disposeChecker.Disposed |> shouldEqual true
      r |> shouldEqual (Error "error")
    )

[<Test>]
let ``either on Ok should run first function``() =
     let x = "dog"
     let r : Result<string, string> = Ok x
     Result.either id id r |> shouldEqual x

[<Test>]
let ``either on Error should run second function``() =
     let x = "dog"
     let r : Result<string, string> = Error x
     Result.either id id r |> shouldEqual x

[<Test>]
let ``defaultValue should return content on Ok``() =
    Result.defaultValue "cat" (Ok "dog") |> shouldEqual "dog"

[<Test>]
let ``defaultValue should return default value on Error``() =
    Result.defaultValue "dog" (Error 42) |> shouldEqual "dog"

[<Test>]
let ``defaultWith should return default value on Error``() =
    Result.defaultWith (konst "dog") (Error 42) |> shouldEqual "dog"

[<Test>]
let ``defaultWith should return content on OK and don't execute the function``() =
    let mutable ``f was run`` = false
    let f () =
        ``f was run`` <- true
        "cat"
    Assert.Multiple
         (fun () ->
            Result.defaultWith f (Ok "dog") |> shouldEqual "dog"
            ``f was run`` |> shouldEqual false
         )
