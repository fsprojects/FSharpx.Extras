module FSharpx.TypeProviders.Graph.Tests.BorderGatewayProtocolGraphTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders

type BGP = Graph<"BorderGatewayProtocol.dgml"> 

[<Test>]
let ``Initially everything is idle``() =   
    let state = BGP.StartFromIdle()
    Assert.AreEqual(state.Name,"Idle")

[<Test>]
let ``Connection can be established``() =   
    let state = 
        BGP.StartFromIdle()
          .TransitToConnect()
          .TransitToOpenSent()
          .TransitToOpenConfirm()
          .TransitToEstablished()
    Assert.AreEqual(state.Name,"Established")