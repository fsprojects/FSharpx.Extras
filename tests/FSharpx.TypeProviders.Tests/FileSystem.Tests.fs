module FSharp.TypeProviders.Tests.FileSystemTests

open FSharpx
open NUnit.Framework
open FSharpx.TypeProviders.JSON
open FsUnit

type T = FileSystemTyped< @"C:\Users\">

[<Test>] 
let ``Can create type for users path``() = 
    T.Path |> should equal @"C:\Users\"