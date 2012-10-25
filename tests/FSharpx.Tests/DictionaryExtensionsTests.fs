module FSharpx.Tests.DictionaryExtensionsTest

open FSharpx
open NUnit.Framework
open FsUnit

[<Test>]
let ``dictionary tryfind with some``() =
  let a = dict [1,"one"; 2,"two"]
  let v = a |> Dictionary.tryFind 1
  match v with
  | Some a -> a |> should equal "one" 
  | None -> failwith "key should have been found"

[<Test>]
let ``dictionary tryfind with none``() =
  let a = dict [1,"one"; 2,"two"]
  let v = a |> Dictionary.tryFind 3
  v |> should sameAs None

[<Test>]
let ``dictionary insertWith``() =
  let a = dict [1,"one"; 2,"two"]
  let v = a |> Dictionary.insertWith (+) 1 "new " |> Dictionary.toList
  v |> should equal [1,"new one"; 2,"two"]

[<Test>]
let ``dictionary updateWith should update value if (f x) is Some``() =
  let f x = if x = "one" then Some "new one" else None
  let a = dict [1,"one"; 2,"two"]
  let v = a |> Dictionary.updateWith f 1 |> Dictionary.toList
  v |> should equal [1,"new one"; 2,"two"]

[<Test>]
let ``dictionary updateWith should delete element if (f x) is None``() =
  let f x = if x = "one" then Some "new one" else None
  let a = dict [1,"one"; 2,"two"]
  let v = a |> Dictionary.updateWith f 2 |> Dictionary.toList
  v |> should equal [1,"one"]

[<Test>]
let ``test Dictionary_splitWithKey correctly breaks the dictionary on the specified predicate``() =
  let a = dict [0,"zero"; 1,"one"; 2,"two"; 3,"three"; 4,"four"]
  let v1,v2 = a |> Dictionary.splitWithKey ((>=) 2)
  v1 |> Dictionary.toList |> should equal [0,"zero"; 1,"one"; 2,"two"]
  v2 |> Dictionary.toList |> should equal [3,"three"; 4,"four"]

[<Test>]
let ``test Dictionary_spanWithKey correctly breaks the dictionary on the specified predicate``() =
  let a = dict [0,"zero"; 1,"one"; 2,"two"; 3,"three"; 4,"four"]
  let v1,v2 = a |> Dictionary.spanWithKey ((<) 2)
  v1 |> Dictionary.toList |> should equal [0,"zero"; 1,"one"; 2,"two"]
  v2 |> Dictionary.toList |> should equal [3,"three"; 4,"four"]
