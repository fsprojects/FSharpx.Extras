module FSharpx.Tests.MaybeTest

open FSharpx.Option
open NUnit.Framework
open FsUnit

let divide x y =
  match y with
  | 0.0 -> None
  | _ -> Some(x/y)

let totalResistance r1 r2 r3 =
  maybe {
    let! x = divide 1.0 r1
    let! y = divide 1.0 r2
    let! z = divide 1.0 r3
    return! divide 1.0 (x + y + z) }

let calculating r1 r2 r3 =
  defaultArg (totalResistance r1 r2 r3) 0.0

[<Test>]
let ``When calculating, it should calculate 0.009581881533``() =
  calculating 0.01 0.75 0.33 |> should equal (1.0/(1.0/0.01+1.0/0.75+1.0/0.33))

[<Test>]
let ``When calculating, it should calculate 0.0``() =
  calculating 0.00 0.55 0.75 |> should equal 0.0 
