module FSharpx.TypeProviders.Tests.AsyncStateMachineTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders
open FsUnit
open System.Threading

type SM1 = AsyncStateMachine<"Graph1.dgml", "State0">

let sleep() = Thread.Sleep 500

[<Test>]
let ``Can access valid states``() =   
    let workflow1 = SM1()
    Assert.AreEqual("State0",workflow1.CurrentState)

    workflow1.TransitToState1()
    sleep()
    Assert.AreEqual("State1",workflow1.CurrentState)

    workflow1.TransitToState2()
    sleep()
    Assert.AreEqual("State2",workflow1.CurrentState)

    workflow1.TransitToState3()
    sleep()
    Assert.AreEqual("State3",workflow1.CurrentState)

[<Test>]
let ``Can't access invalid states``() =
    let workflow1 = SM1()
    workflow1.TransitToState1()
    workflow1.TransitToState2()
    workflow1.TransitToState3()
    sleep()
    Assert.AreEqual("State3",workflow1.CurrentState)

    workflow1.TransitToState2()
    sleep()
    Assert.AreEqual("State3",workflow1.CurrentState)
