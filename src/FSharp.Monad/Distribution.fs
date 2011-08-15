module FSharp.Monad.Distribution

type 'a Outcome = {
    Value: 'a
    Probability : BigRational  }

type 'a Distribution = 'a Outcome seq

// P(A AND B) = P(A | B) * P(B)
let bindD (dist:'a Distribution) (f: 'a -> 'b Distribution) =
    dist 
        |> Seq.map (fun p1 -> 
                f p1.Value
                |> Seq.map (fun p2 -> 
                        { Value = p2.Value; 
                            Probability = 
                              p1.Probability * 
                              p2.Probability}))
        |> Seq.concat : 'b Distribution

let returnD (value:'a) =   
    Seq.singleton { Value = value ; Probability = 1N/1N }
      : 'a Distribution

type DistributionMonadBuilder() =
    member this.Bind (r, f) = bindD r f
    member this.Return x = returnD x

let distribution = DistributionMonadBuilder()

// Create some helpers
let toUniformDistribution seq : 'a Distribution =
    let l = Seq.length seq
    seq 
      |> Seq.map (fun e -> 
            { Value = e; 
              Probability = 1N / bignum.FromInt l })

let probability (dist:'a Distribution) = 
    dist
      |> Seq.map (fun o -> o.Probability)
      |> Seq.sum

let fairDice sides = toUniformDistribution [1..sides]

type CoinSide = 
| Heads 
| Tails

let fairCoin = toUniformDistribution [Heads; Tails]

let filter predicate (dist:'a Distribution) =
    dist |> Seq.filter (fun o -> predicate o.Value)