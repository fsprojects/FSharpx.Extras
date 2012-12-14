module FSharpx.TypeProviders.Tests.FreebaseTests.Chemistry

open NUnit.Framework
open FsUnit
open FSharpx.TypeProviders.Freebase

let data = FreebaseData.GetDataContext()

let elements = data.``Science and Technology``.Chemistry.``Chemical Elements``

[<Test>]
let ``Can access the symbol for hydrogen``() =
    let hydrogen = elements.Individuals.Hydrogen
    hydrogen.Symbol |> should equal "H"