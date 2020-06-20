module FSharpx.Tests.ResultOfOptionTests

open NUnit.Framework
open FSharpx
open FsUnitTyped

[<Test>]
let ``can create Ok from ofOption``() =
  Some "Success"
  |> Result.ofOption ""
  |> shouldEqual (Ok "Success")

[<Test>]
let ``can create Error from ofOption``() =
  None
  |> Result.ofOption "Failure"
  |> shouldEqual (Error "Failure")

[<Test>]
let ``can create Ok from ofOptionF``() =
  Some "Success"
  |> Result.ofOptionF (fun () -> "")
  |> shouldEqual (Ok "Success")

[<Test>]
let ``can create Error from ofOptionF``() =
  None
  |> Result.ofOptionF (fun () -> "Failure")
  |> shouldEqual (Error "Failure")