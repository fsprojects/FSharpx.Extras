module FSharpx.Tests.SeqTests

open FSharpx
open NUnit.Framework

[<Test>]
let index() =
  let a = {'a'..'z'}
  let i = Seq.index a |> Seq.take 5 |> Seq.toList
  Assert.AreEqual([0,'a'; 1,'b'; 2,'c'; 3,'d'; 4,'e'], i)
  ()

[<Test>]
let tryFindWithIndex_None() =
  let a = {1..10}
  let r = Seq.tryFindWithIndex ((<)10) a
  Assert.AreEqual(None, r)

[<Test>]
let tryFindWithIndex_Some() =
  let a = {'a'..'z'}
  let r = Seq.tryFindWithIndex ((=)'e') a
  match r with
  | Some e -> Assert.AreEqual((4,'e'), e)
  | _ -> failwith "Not found"
