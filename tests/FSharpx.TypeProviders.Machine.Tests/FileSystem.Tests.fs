module FSharpx.TypeProviders.Tests.FileSystemTests

open FSharpx
open NUnit.Framework
open FsUnit

type Users = FileSystem<"C:\\Users\\">

[<Test>] 
let ``Can create type for users path``() = 
    Users.Path |> should equal @"C:\Users\"

[<Test>] 
let ``Can access the default users path``() = 
    Users.Default.Path |> should equal @"C:\Users\Default"