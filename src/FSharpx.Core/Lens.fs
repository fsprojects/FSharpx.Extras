namespace FSharpx

type Lens<'a,'b> = {
    Get: 'a -> 'b
    Set: 'b -> 'a -> 'a
}

module Lens =
    let get a (l: Lens<_,_>) = l.Get a
    let set v a (l: Lens<_,_>) = l.Set v a
    let update f a (l: Lens<_,_>) = l.Set (f(l.Get a)) a

    let compose (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        { Get = fun a -> l1.Get (l2.Get a)
          Set = fun b a -> update (l1.Set b) a l2 }

    let inline (.*.) l1 l2 = compose l1 l2

    let fst =
        { Get = Operators.fst
          Set = fun v a -> v, Operators.snd a }

    let snd =
        { Get = Operators.snd
          Set = fun v a -> Operators.fst a, v }

    let id = 
        { Get = Operators.id
          Set = fun _ a -> a }

    let forSet value =
        { Get = Set.contains value
          Set = fun contains -> (if contains then Set.add else Set.remove) value }

    let forMap key = 
        { Get = Map.tryFind key
          Set = 
            function
            | Some value -> Map.add key value
            | None -> Map.remove key }

    let ignore = 
        { Get = ignore
          Set = fun _ v -> v }
