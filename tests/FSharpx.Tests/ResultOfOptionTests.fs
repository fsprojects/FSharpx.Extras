module FSharpx.Tests.ResultOfOptionTests

open NUnit.Framework
open FSharpx

let equals expected actual =
    if expected <> actual then Assert.Fail(sprintf "Expected %A to equal %A" actual expected)

[<Test>]
let ``can create Ok from ofOption``() =
  Some "Success"
  |> Result.ofOption ""
  |> equals (Ok "Success")

[<Test>]
let ``can create Error from ofOption``() =
  None
  |> Result.ofOption "Failure"
  |> equals (Error "Failure")

[<Test>]
let ``can create Ok from ofOptionF``() =
  Some "Success"
  |> Result.ofOptionF (fun () -> "")
  |> equals (Ok "Success")

[<Test>]
let ``can create Error from ofOptionF``() =
  None
  |> Result.ofOptionF (fun () -> "Failure")
  |> equals (Error "Failure")