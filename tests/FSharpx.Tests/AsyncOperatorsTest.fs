module FSharpx.Tests.AsyncOperatorsTest

open FSharpx
open FSharpx.Async
open NUnit.Framework
open FsUnit

[<Test>]
let ``test >>= should bind an async``() =
  let test = async.Return 1
  let binder x = async.Return(x.ToString())
  let expected = async.Bind(test, binder) |> Async.RunSynchronously
  test >>= binder |> Async.RunSynchronously |> should equal expected

[<Test>]
let ``test <*> should apply an Async<'a -> 'b> to an Async<'a>``() =
  let test = async.Return 2
  let func = async.Return(fun x -> x * x)
  func <*> test |> Async.RunSynchronously |> should equal 4

[<Test>]
let ``test pipe should map an Async<'a> to an Async<'b>``() =
  let test = async.Return 2
  let mapper x = x * x
  pipe test mapper |> Async.RunSynchronously |> should equal 4

[<Test>]
let ``test <!> should map an Async<'a> to an Async<'b>``() =
  let test = async.Return 2
  let mapper x = x * x
  mapper <!> test |> Async.RunSynchronously |> should equal 4

[<Test>]
let ``test pipe2 should map an Async<'a> and Async<'b> to an Async<'c>``() =
  let a = async.Return 2
  let b = async.Return 3
  pipe2 a b (+) |> Async.RunSynchronously |> should equal 5

[<Test>]
let ``test pipe3 should map an Async<'a>, Async<'b>, and Async<'c> to an Async<'d>``() =
  let a = async.Return 2
  let b = async.Return 3
  let c = async.Return 4
  let mapper x y z = x + y + z
  pipe3 a b c mapper |> Async.RunSynchronously |> should equal 9

[<Test>]
let ``test <* takes only the value from the first Async``() =
  let a = async.Return 2
  let b = async.Return 3
  a <* b |> Async.RunSynchronously |> should equal 2

[<Test>]
let ``test *> takes only the value from the second Async``() =
  let a = async.Return 2
  let b = async.Return 3
  a *> b |> Async.RunSynchronously |> should equal 3

[<Test>]
let ``test <!> and <*> should enable zip``() =
  let a = async.Return 2
  let b = async.Return 3
  let zip a b = (fun a b -> a,b) <!> a <*> b
  zip a b |> Async.RunSynchronously |> should equal (2,3)

[<Test>]
let ``test >>. should apply both arguments and return the result of the last``() =
  let a = async.Return 2
  let b = async.Return 3
  a >>. b |> Async.RunSynchronously |> should equal 3

(* Test callcc *)
let sum l =
  let rec sum l = async {
    let! result = callcc (fun exit1 -> async {
      match l with
      | [] -> return 0
      | h::t when h = 2 -> return! exit1 42
      | h::t -> let! r = sum t
                return h + r })
    return result }
  Async.RunSynchronously(sum l)

[<Test>]
let ``When summing a list without a 2 via callcc it should return 8``() =
  sum [1;1;3;3] |> should equal 8

[<Test>]
let ``When summing a list containing 2 via callcc it should return 43``() =
  sum [1;2;3] |> should equal 43
