namespace FSharpx

[<AbstractClass>]
type Lens<'a,'b>() =
    abstract Get : 'a -> 'b
    abstract Set : 'b -> 'a -> 'a
    abstract Mod: ('b -> 'b) -> 'a -> 'a
    default x.Mod f a = x.Set (f(x.Get a)) a
    abstract Compose : Lens<'c,'a> -> Lens<'c,'b>
    default x.Compose l = 
        { new Lens<_,_>() with
            override y.Get a = x.Get (l.Get a)
            override y.Set b a = l.Mod (x.Set b) a }

module Lens =
    let get a (l: Lens<_,_>) = l.Get a
    let set v a (l: Lens<_,_>) = l.Set v a
    let fst<'a,'b> =
        { new Lens<'a * 'b,'a>() with
            override x.Get a = Operators.fst a
            override x.Set v a = v, Operators.snd a }
    let snd<'a,'b> =
        { new Lens<'a * 'b,'b>() with
            override x.Get a = Operators.snd a
            override x.Set v a = Operators.fst a, v }
    let id<'a> = 
        { new Lens<'a,'a>() with
            override x.Get a = a
            override x.Set _ a = a }

    let forSet value =
        { new Lens<'a Set, bool>() with
            override x.Get set = 
                Set.contains value set
            override x.Set contains set = 
                (if contains then Set.add else Set.remove) value set }

    let forMap key = 
        { new Lens<Map<'k,'v>, 'v option>() with
            override x.Get map = 
                Map.tryFind key map
            override x.Set value map = 
                match value with
                | Some value -> Map.add key value map
                | None -> Map.remove key map }

    // doesn't compile for some reason
//        let ignore<'a> = 
//            { new Lens<'a,unit>() with
//                override x.Get a = ()
//                override x.Set _ v = v }