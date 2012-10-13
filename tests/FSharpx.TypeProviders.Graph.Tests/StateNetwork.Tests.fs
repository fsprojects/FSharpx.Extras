module FSharpx.TypeProviders.Graph.Tests.StateNetworkTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders

type Graph1 = Graph<"Graph1.dgml">

[<Test>]
let ``Can access initial state``() =   
    let state = Graph1.StartFromState0()
    Assert.AreEqual(state.Name,"State0")

[<Test>]
let ``Can access states``() =   
    let state2 = 
        Graph1.StartFromState0()
          .TransitToState1()
          .TransitToState2()

    Assert.AreEqual(state2.Name,"State2")