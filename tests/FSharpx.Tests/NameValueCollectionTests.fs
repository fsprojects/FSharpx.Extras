module FSharpx.Tests.NameValueCollectionTests

open FsUnit
open FSharpx.Collections
open NUnit.Framework
open System.Collections.Generic
open System.Collections.Specialized
open System.Linq

[<Test>]
let ofSeq() =
  let n1 = NameValueCollection.ofSeq ["1","uno"; "1","one"; "2","two"]
  let n2 = NameValueCollection()
  n2.Add("1", "uno")
  n2.Add("1", "one")
  n2.Add("2", "two")
  Assert.AreEqual(n1,n2)

[<Test>]
let toSeq() =
  let r = ["1","uno"; "1","one"; "2","two"]
  let a = NameValueCollection.ofSeq r
  let s = NameValueCollection.toSeq a
  Assert.AreEqual(r,s)

[<Test>]
let toArray() =
  let r = [|"1","uno"; "1","one"; "2","two"|]
  let a = NameValueCollection.ofSeq r
  let s = NameValueCollection.toArray a
  Assert.AreEqual(r,s)
  
[<Test>]
let toList() =
  let r = ["1","uno"; "1","one"; "2","two"]
  let a = NameValueCollection.ofSeq r
  let s = NameValueCollection.toList a
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

let assertKeyIs (l: ILookup<_,_>) a key = 
  Assert.AreEqual(a, l.[key] |> Seq.toList)

[<Test>]
let toLookup() =
  let n1 = NameValueCollection.ofSeq ["1","uno"; "1","one"; "2","two"]
  let l = NameValueCollection.toLookup n1
  "1" |> assertKeyIs l ["uno"; "one"]
  "2" |> assertKeyIs l ["two"]
  "3" |> assertKeyIs l []
  n1.Add("3", "three")
  "3" |> assertKeyIs l []

[<Test>]
let asLookup() =
  let n1 = NameValueCollection.ofSeq ["1","uno"; "1","one"; "2","two"]
  let l = NameValueCollection.asLookup n1
  "1" |> assertKeyIs l ["uno"; "one"]
  "2" |> assertKeyIs l ["two"]
  "3" |> assertKeyIs l []
  n1.Add("3", "three")
  "3" |> assertKeyIs l ["three"]

[<Test>]
let asDictionary() =
  let n1 = NameValueCollection.ofSeq ["1","uno"; "1","one"; "2","two"]
  let d = NameValueCollection.asDictionary n1
  Assert.AreEqual([|"uno";"one"|], d.["1"])
  Assert.AreEqual([|"two"|], d.["2"])
  (fun () -> ignore d.["3"]) |> should throw typeof<KeyNotFoundException>
  n1.Add("3", "three")
  Assert.AreEqual([|"three"|], d.["3"])