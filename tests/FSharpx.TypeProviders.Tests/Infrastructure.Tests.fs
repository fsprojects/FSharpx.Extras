module FSharpx.TypeProviders.Tests.InfrastructureTests

open NUnit.Framework
open FSharpx
open FsUnit
open FSharpx.TypeProviders.DSL
open FSharpx.TypeProviders.Inference

[<Test>]
let ``Can infer floats``() = 
    isFloat "42.42" |> should equal true