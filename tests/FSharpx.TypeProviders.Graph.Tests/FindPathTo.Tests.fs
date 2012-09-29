module FSharpx.TypeProviders.Graph.Tests.FindPathToTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders

type Actors = Graph<"Actors.dgml", "Keanu Reeves">

[<Test>]
let ``Can find path between Keanu Reeves and Kevin Bacon``() =   
    let path = Actors.StartFromKeanuReeves().ShortestPathToKevinBacon()
    Assert.AreEqual(["The Matrix"; "Laurence Fishburne"; "Mystic River"],path)