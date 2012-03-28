module FSharpx.TypeProviders.Tests.FileSystemTests

open FSharpx
open NUnit.Framework
open FsUnit

type T = FileSystemTyped< @"C:\Users\">

[<Test>] 
let ``Can create type for users path``() = 
    T.Path |> should equal @"C:\Users\"