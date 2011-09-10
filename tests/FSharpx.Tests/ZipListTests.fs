module FSharpx.Tests.ZipListTests

open FsUnit
open NUnit.Framework
open FSharpx
open FSharpx.Tuples

[<Test>]
let zipping() =
  let z = 
    ZipList.returnM t3
    |> ZipList.ap [1;2;3]
    |> ZipList.ap ["one";"two";"three"]
    |> ZipList.ap ["uno";"dos";"tres"]
  Assert.AreEqual([1,"one","uno"; 2,"two","dos"; 3,"three","tres"], z)