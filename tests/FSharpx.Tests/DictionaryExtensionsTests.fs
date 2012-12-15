module FSharpx.Tests.DictionaryExtensionsTest

open FSharpx.Collections
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
