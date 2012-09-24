module FSharpx.TypeProviders.Tests.StateMachineTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders
open FsUnit

type SM1 = StateMachine<"Graph1.dgml", "State0">

[<Test>]
let ``Can access valid states``() =   
    let workflow1 = SM1()
    Assert.AreEqual("State0",workflow1.CurrentState)

    workflow1.TransitToState1()
    Assert.AreEqual("State1",workflow1.CurrentState)

    workflow1.TransitToState2()
    Assert.AreEqual("State2",workflow1.CurrentState)

    workflow1.TransitToState3()
    Assert.AreEqual("State3",workflow1.CurrentState)

[<Test>]
let ``Can't access invalid states``() =
    let workflow1 = SM1()
    workflow1.TransitToState1()
    workflow1.TransitToState2()
    workflow1.TransitToState3()
    Assert.AreEqual("State3",workflow1.CurrentState)

    workflow1.TransitToState2()
    Assert.AreEqual("State3",workflow1.CurrentState)


[<Test>]
let ``Executes the transition functions``() =   
    let workflow1 = SM1()

    let entered1 = ref false
    let left1 = ref false
    let entered2 = ref false
    let left2 = ref false

    let trans1 = { new StateMachine.IState with
                            member this.EnterFunction() = entered1 := true
                            member this.ExitFunction() = left1 := true }

    let trans2 = { new StateMachine.IState with
                            member this.EnterFunction() = entered2 := true
                            member this.ExitFunction() = left2 := true }

    workflow1.SetTransitionFunction(workflow1.State1, trans1)
    workflow1.SetTransitionFunction(workflow1.State2, trans2)

    workflow1.TransitToState1()
    Assert.IsTrue(!entered1)
    Assert.IsFalse(!left1)
    Assert.IsFalse(!entered2)
    Assert.IsFalse(!left2)

    workflow1.TransitToState2()
    Assert.IsTrue(!entered1)
    Assert.IsTrue(!left1)
    Assert.IsTrue(!entered2)
    Assert.IsFalse(!left2)
