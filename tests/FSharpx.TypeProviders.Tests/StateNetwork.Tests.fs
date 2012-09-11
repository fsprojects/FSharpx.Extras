module FSharpx.TypeProviders.Tests.StateNetworkTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders
open FsUnit

type Graph1 = StateNetwork<"Graph1.dgml", "State0">

[<Test>]
let ``Can access initial state``() =   
    let state = Graph1.InitialState
    Assert.AreEqual(state.Name,"State0")

[<Test>]
let ``Can access states``() =   
    let state = Graph1.InitialState
    let state1 = state.TransitToState1()
    Assert.AreEqual(state1.Name,"State1")