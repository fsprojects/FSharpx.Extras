module FSharpx.Tests.MapExtensionsTest

open FSharpx.Collections
open NUnit.Framework
open FsUnit

[<Test>]
let ``map insertWith``() =
  let a = Map.ofList [1,"one"; 2,"two"]
  let v = a |> Map.insertWith (+) 1 "new " |> Map.toList
  v |> should equal [1,"new one"; 2,"two"]

[<Test>]
let ``map updateWith should update value if (f x) is Some``() =
  let f x = if x = "one" then Some "new one" else None
  let a = Map.ofList [1,"one"; 2,"two"]
  let v = a |> Map.updateWith f 1 |> Map.toList
  v |> should equal [1,"new one"; 2,"two"]

[<Test>]
let ``map updateWith should delete element if (f x) is None``() =
  let f x = if x = "one" then Some "new one" else None
  let a = Map.ofList [1,"one"; 2,"two"]
  let v = a |> Map.updateWith f 2 |> Map.toList
  v |> should equal [1,"one"]

[<Test>]
let ``test Map_splitWithKey correctly breaks the dictionary on the specified predicate``() =
  let a = Map.ofList [0,"zero"; 1,"one"; 2,"two"; 3,"three"; 4,"four"]
  let v1,v2 = a |> Map.splitWithKey ((>=) 2)
  v1 |> Map.toList |> should equal [0,"zero"; 1,"one"; 2,"two"]
  v2 |> Map.toList |> should equal [3,"three"; 4,"four"]

[<Test>]
let ``test Map_spanWithKey correctly breaks the dictionary on the specified predicate``() =
  let a = Map.ofList [0,"zero"; 1,"one"; 2,"two"; 3,"three"; 4,"four"]
  let v1,v2 = a |> Map.spanWithKey ((<) 2)
  v1 |> Map.toList |> should equal [0,"zero"; 1,"one"; 2,"two"]
  v2 |> Map.toList |> should equal [3,"three"; 4,"four"]
