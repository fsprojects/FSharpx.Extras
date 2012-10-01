module FSharpx.TypeProviders.Graph.Tests.TurnstileGraphTests

open NUnit.Framework
open FSharpx
open FSharpx.TypeProviders

type Turnstile = Graph<"Turnstile.dgml"> 

[<Test>]
let ``Turnstile should be locked initially``() =   
    let state = Turnstile.StartFromLocked()
    Assert.AreEqual(state.Name,"Locked")

[<Test>]
let ``Pushing doesn't unlock the turnstile``() =   
    let state = Turnstile.StartFromLocked().Push()
    Assert.AreEqual(state.Name,"Locked")

[<Test>]
let ``Insert a coin and the turnstile is unlocked``() =   
    let state = Turnstile.StartFromLocked().Coin()
    Assert.AreEqual(state.Name,"Unlocked")

[<Test>]
let ``Insert a coin and push then the turnstile is locked``() =   
    let state = 
      Turnstile.StartFromLocked().Coin().Push()
    Assert.AreEqual(state.Name,"Locked")

[<Test>]
let ``Insert two coins and push then the turnstile is locked``() =   
    let state = 
      Turnstile.StartFromLocked().Coin().Coin().Push()
    Assert.AreEqual(state.Name,"Locked")