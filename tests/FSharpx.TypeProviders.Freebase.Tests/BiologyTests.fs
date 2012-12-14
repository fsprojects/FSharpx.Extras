module FSharpx.TypeProviders.Tests.FreebaseTests.Biology

open NUnit.Framework
open FsUnit
open FSharpx.TypeProviders.Freebase
open System.Linq
open System

let data = FreebaseData.GetDataContext()

let aminoAcids = data.``Science and Technology``.Biology.``Amino Acids``

[<Test>]
let ``Can access the first 10 amino acids``() =
    let q = query {
        for acid in aminoAcids do
        take 10
        select (acid.Name, String.Join(" ", acid.Blurb.ToArray())) }
    let a = q.ToArray()
    a.Count() |> should equal 10