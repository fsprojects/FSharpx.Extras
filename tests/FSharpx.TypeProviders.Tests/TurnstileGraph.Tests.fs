module FSharpx.TypeProviders.Tests.TurnstileGraphTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders
open FsUnit

type Turnstile = Graph<"Turnstile.dgml", "Locked"> 

[<Test>]
let ``Turnstile should be locked initially``() =   
    let state = Turnstile.InitialState
    Assert.AreEqual(state.Name,"Locked")

[<Test>]
let ``Pushing doesn't unlock the turnstile``() =   
    let state = Turnstile.InitialState.Push()
    Assert.AreEqual(state.Name,"Locked")

[<Test>]
let ``Insert a coin and the turnstile is unlocked``() =   
    let state = Turnstile.InitialState.Coin()
    Assert.AreEqual(state.Name,"Unlocked")

[<Test>]
let ``Insert a coin and push then the turnstile is locked``() =   
    let state = 
      Turnstile.InitialState.Coin().Push()
    Assert.AreEqual(state.Name,"Locked")

[<Test>]
let ``Insert two coins and push then the turnstile is locked``() =   
    let state = 
      Turnstile.InitialState.Coin().Coin().Push()
    Assert.AreEqual(state.Name,"Locked")