module FSharpx.TypeProviders.Graph.Tests.BinaryGraphTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders

type BinaryGraph = Graph<"Binary.dgml", "Even"> 

[<Test>]
let ``Empty string should be even``() =   
    let state = BinaryGraph.InitialState
    Assert.AreEqual(state.Name,"Even")

[<Test>]
let ``Single 1 should be even``() =   
    let state = BinaryGraph.InitialState.``1``()
    Assert.AreEqual(state.Name,"Even")

[<Test>]
let ``10 should be odd``() =   
    let state = BinaryGraph.InitialState.``1``().``0``()
    Assert.AreEqual(state.Name,"Odd")

[<Test>]
let ``101010 should be odd``() =   
    let state = 
      BinaryGraph.InitialState
        .``1``().``0``().``1``().``0``().``1``().``0``()
    Assert.AreEqual(state.Name,"Odd")

[<Test>]
let ``101011 should be even``() =   
    let state = 
      BinaryGraph.InitialState
        .``1``().``0``().``1``().``0``().``1``().``1``()
    Assert.AreEqual(state.Name,"Even")