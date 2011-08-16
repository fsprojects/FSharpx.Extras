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
                            p1.Probability * p2.Probability}))
        |> Seq.concat : 'b Distribution

let inline (>>=) dist f = bindD dist f

let returnD (value:'a) =   
    Seq.singleton { Value = value ; Probability = 1N/1N }
      : 'a Distribution

type DistributionMonadBuilder() =
    member this.Bind (r, f) = bindD r f
    member this.Return x = returnD x
    member this.ReturnFrom x = x

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

let certainly = returnD
let impossible<'a> :'a Distribution = toUniformDistribution []

let fairDice sides = toUniformDistribution [1..sides]

type CoinSide = 
| Heads 
| Tails

let fairCoin = toUniformDistribution [Heads; Tails]

let filter predicate (dist:'a Distribution) : 'a Distribution =
    dist |> Seq.filter (fun o -> predicate o.Value)

let filterInAnyOrder items dist =
  items
    |> Seq.fold (fun d item -> filter (Seq.exists ((=) (item))) d) dist

let map f (dist:'a Distribution) : 'b Distribution = 
    dist 
      |> Seq.map (fun o -> { Value = f o.Value; Probability = o.Probability })

let selectOne values =
    [for e in values -> e,values |> Seq.filter ((<>) e)] 
      |> toUniformDistribution

let rec selectMany n values =
    match n with 
    | 0 -> certainly ([],values)
    | _ -> 
        distribution {
             let! (x,c1) = selectOne values
             let! (xs,c2) = selectMany (n-1) c1
             return x::xs,c2}
    
let select n values = 
    selectMany n values   
      |> map (fst >> List.rev)

let remove items = Seq.filter (fun v -> Seq.forall ((<>) v) items)