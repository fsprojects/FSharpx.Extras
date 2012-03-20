module FSharpx.Tests.ZipListTests

open FsUnit
open NUnit.Framework
open FSharpx
open ZipList

[<Test>]
let zipping() =
  let (ZipList z) = 
    pure' tuple3
    <*> (ZipList [1;2;3])
    <*> (ZipList ["one";"two";"three"])
    <*> (ZipList ["uno";"dos";"tres"])
  Assert.AreEqual([1,"one","uno"; 2,"two","dos"; 3,"three","tres"], z)