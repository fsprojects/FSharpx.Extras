module FSharpx.TypeProviders.Tests.JsonZipper.Array.ZipperTests

open NUnit.Framework
open FSharpx
open FsUnit

//type SimpleArrayOfInts = JsonZipper<Schema="""{"a":[1,2,3]}""">
//
//[<Test>]
//let ``Can access an int in a simple JSON array``() = 
//    let original = new SimpleArrayOfInts()
//    let updated = original.A.GetElement(0).Update(6)
//    updated.ToString() |> should equal """{"a":[6,2,3]}"""
//
//    let updated = original.A.GetElement(2).Update(42)
//    updated.ToString() |> should equal """{"a":[1,2,42]}"""
//
//    original.ToString() |> should equal """{"a":[1,2,3]}"""
//
//
//type SimpleArrayOfStrings = JsonZipper<Schema="""{"Guys":["Tom","Jerry","Max","Moritz"]}""">
//
//[<Test>]
//let ``Can access a string in a simple JSON array``() = 
//    let original = new SimpleArrayOfStrings()
//    let updated = original.Guys.GetElement(0).Update("Hulk")
//    updated.ToString() |> should equal """{"Guys":["Hulk","Jerry","Max","Moritz"]}"""
//
//    let updated = original.Guys.GetElement(2).Update("Mickey")
//    updated.ToString() |> should equal """{"Guys":["Tom","Jerry","Mickey","Moritz"]}"""
//
//    original.ToString() |> should equal """{"Guys":["Tom","Jerry","Max","Moritz"]}"""