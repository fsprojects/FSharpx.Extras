module FSharpx.TypeProviders.Tests.StateNetworkTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders
open FsUnit

type Graph1 = Graph<"Graph1.dgml", "State0">

[<Test>]
let ``Can access initial state``() =   
    let state = Graph1.InitialState
    Assert.AreEqual(state.Name,"State0")

[<Test>]
let ``Can access states``() =   
    let state2 = 
        Graph1.InitialState
          .TransitToState1()
          .TransitToState2()

    Assert.AreEqual(state2.Name,"State2")