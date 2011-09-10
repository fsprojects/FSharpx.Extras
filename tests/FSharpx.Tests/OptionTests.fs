module FSharpx.Tests.OptionTests

open NUnit.Framework
open FSharpx
open FSharpx.Option

[<Test>]
let ``kleisli composition``() =
  let f x = 
    if x > 5
      then Some "hello"
      else None
  let g x =
    if x = "hello"
      then Some 10
      else None

  let h = f >=> g
  Assert.AreEqual(Some 10, h 8)
  Assert.AreEqual(None, h 1)
  ()

