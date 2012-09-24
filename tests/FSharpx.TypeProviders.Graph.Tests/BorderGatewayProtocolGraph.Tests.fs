module FSharpx.TypeProviders.Graph.Tests.BorderGatewayProtocolGraphTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders

type BGP = Graph<"BorderGatewayProtocol.dgml", "Idle"> 

[<Test>]
let ``Initially everything is idle``() =   
    let state = BGP.InitialState
    Assert.AreEqual(state.Name,"Idle")

[<Test>]
let ``Connection can be established``() =   
    let state = 
        BGP.InitialState
          .TransitToConnect()
          .TransitToOpenSent()
          .TransitToOpenConfirm()
          .TransitToEstablished()
    Assert.AreEqual(state.Name,"Established")