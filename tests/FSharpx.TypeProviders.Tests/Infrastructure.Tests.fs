module FSharp.TypeProviders.Tests.InfrastructureTests

open NUnit.Framework
open FSharpx
open FsUnit
open FSharpx.TypeProviders.DSL

[<Test>]
let ``Can simplify the type names``() = 
    let (=!=) a b = a |> should equal b

    niceName "" =!= "" 
    niceName "__hello__" =!= "Hello"
    niceName "abc" =!= "Abc"
    niceName "hello_world" =!= "HelloWorld"
    niceName "HelloWorld" =!= "HelloWorld"
    niceName "helloWorld" =!= "HelloWorld"
    niceName "hello123" =!= "Hello123"
    niceName "Hello123" =!= "Hello123"
    niceName "hello!123" =!= "Hello123"
    niceName "HelloWorld123_hello__@__omg" =!= "HelloWorld123HelloOmg"