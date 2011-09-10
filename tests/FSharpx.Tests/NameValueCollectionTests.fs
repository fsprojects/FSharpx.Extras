module FSharpx.Tests.NameValueCollectionTests

open FSharpx
open NUnit.Framework
open System.Collections.Specialized

[<Test>]
let fromSeq() =
  let n1 = NameValueCollection.fromSeq ["1","uno"; "1","one"; "2","two"]
  let n2 = NameValueCollection()
  n2.Add("1", "uno")
  n2.Add("1", "one")
  n2.Add("2", "two")
  Assert.AreEqual(n1,n2)

[<Test>]
let toSeq() =
  let r = ["1","uno"; "1","one"; "2","two"]
  let a = NameValueCollection.fromSeq r
  let s = NameValueCollection.toSeq a
  Assert.AreEqual(r,s)

[<Test>]
let concat() =
  let a = NameValueCollection()
  a.Add("1", "uno")
  a.Add("2", "dos")
  let b = NameValueCollection()
  b.Add("1", "one")
  b.Add("2", "two")
  let c = NameValueCollection.concat a b
  ()

[<Test>]
let toLookup() =
  let n1 = NameValueCollection.fromSeq ["1","uno"; "1","one"; "2","two"]
  let l = NameValueCollection.toLookup n1
  let assertKeyIs a (key: string) = Assert.AreEqual(a, l.[key] |> Seq.toList)
  "1" |> assertKeyIs ["uno"; "one"]
  "2" |> assertKeyIs ["two"]
  "3" |> assertKeyIs []
