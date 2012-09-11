module FSharpx.TypeProviders.Tests.StateMachineTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders
open FsUnit

type SM1 = StateMachine<"Graph1.dgml", false, "State0">

[<Test>]
let ``Can access valid states``() =   
    let workflow1 = SM1()
    Assert.AreEqual("State0",workflow1.CurrentState)

    workflow1.TransitTo_State1()
    Assert.AreEqual("State1",workflow1.CurrentState)

    workflow1.TransitTo_State2()
    Assert.AreEqual("State2",workflow1.CurrentState)

    workflow1.TransitTo_State3()
    Assert.AreEqual("State3",workflow1.CurrentState)

[<Test>]
let ``Can access invalid states``() =
    let workflow1 = SM1()
    workflow1.TransitTo_State1()
    workflow1.TransitTo_State2()
    workflow1.TransitTo_State3()
    Assert.AreEqual("State3",workflow1.CurrentState)

    workflow1.TransitTo_State2()
    Assert.AreEqual("State3",workflow1.CurrentState)
