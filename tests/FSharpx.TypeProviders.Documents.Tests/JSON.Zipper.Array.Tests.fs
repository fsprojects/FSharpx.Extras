module FSharpx.TypeProviders.Tests.JsonZipper.Array.ZipperTests

open NUnit.Framework
open FSharpx
open FsUnit


type SimpleArray = JsonZipper<Schema="""{"a":[1,2,3]}""">

[<Test>]
let ``Can access an int in a simple JSON array``() = 
    let original = new SimpleArray()
    let updated = original.GetElement(0).Update(6)
    updated.ToString() |> should equal """{"a":[6,2,3]}"""
    original.ToString() |> should equal """{"a":[1,2,3]}"""