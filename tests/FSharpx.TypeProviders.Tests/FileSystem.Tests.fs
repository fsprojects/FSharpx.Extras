module FSharp.TypeProviders.Tests.FileSystemTests

open FSharpx
open NUnit.Framework
open FSharpx.TypeProviders.JSON
open FsUnit

[<Test>]
let Dummy()=
    1 |> should equal 1

// Unfortunately this makes everything very slow. // TODO: better test
//
//type T = FileSystemTyped< @"C:\Users\">
//
//[<Test>] 
//let ``Can create type for users path``() = 
//    T.Path |> should equal @"C:\Users\"