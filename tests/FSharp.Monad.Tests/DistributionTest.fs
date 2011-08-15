module FSharp.Monad.Tests.DistributionTest

open FSharp.Monad.Distribution
open NUnit.Framework
open FsUnit

[<Test>]
let ``When creating a empty distribution, then the probability should be 1``() =
  let actual = distribution { return () }
  probability actual |> should equal (1N/1N)

let sumOfTwoFairDices = distribution {
  let! d1 = fairDice 6
  let! d2 = fairDice 6
  return d1 + d2 }

[<Test>]
let ``When creating two fair dices, then P(Sum of 2 dices = 7) should be 1/6``() =
  let actual = sumOfTwoFairDices |> filter ((=) 7)
  probability actual |> should equal (1N/6N)

let fairCoinAndDice = distribution {
  let! d = fairDice 6
  let! c = fairCoin
  return d,c }

[<Test>]
let ``When creating a fair coin and a fair dice, then P(Heads) should be 1/2``() =
  let actual = fairCoinAndDice |> filter (fun (_,c) -> c = Heads)
  probability actual |> should equal (1N/2N)

[<Test>]
let ``When creating a fair coin and a fair dice, then P(Heads and dice > 3) should be 1/4``() =
  let actual = fairCoinAndDice |> filter (fun (d,c) -> c = Heads && d > 3)
  probability actual |> should equal (1N/4N)

// MontyHall Problem
// See Martin Erwig and Steve Kollmansberger's paper 
// "Functional Pearls: Probabilistic functional programming in Haskell"

type Outcome = 
| Car
| Goat

let firstChoice = toUniformDistribution [Car; Goat; Goat]

let switch = function
| Car -> certainly Goat
| Goat -> certainly Car

[<Test>]
let ``When making the first choice in a MontyHall situation, the chances to win should be 1/3``() =
  firstChoice 
    |> filter ((=) Car)
    |> probability 
    |> should equal (1N/3N)

[<Test>]
let ``When switching in a MontyHall situation, the chances to win should be 2/3``() =
  firstChoice >>= switch
    |> filter ((=) Car)
    |> probability 
    |> should equal (2N/3N)