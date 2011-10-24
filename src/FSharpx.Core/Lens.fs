namespace FSharpx

type Lens<'a,'b> = {
    Get: 'a -> 'b
    Set: 'b -> 'a -> 'a
} with 
    member l.Update f a = l.Set (f(l.Get a)) a

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =
    let inline get a (l: Lens<_,_>) = l.Get a
    let inline set v a (l: Lens<_,_>) = l.Set v a
    let inline update f (l: Lens<_,_>) = l.Update f

    let inline compose (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        { Get = fun a -> l1.Get (l2.Get a)
          Set = fun b -> l2.Update (l1.Set b) }

    let inline choice (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        { Get = 
            function
            | Choice1Of2 a -> l1.Get a
            | Choice2Of2 a -> l2.Get a
          Set = fun b -> Choice.bimap (l1.Set b) (l2.Set b) }    

    let getState l = 
        fun a -> get a l, a

    let setState l v = 
        fun a -> (), set v a l

    let updateState l f =
        fun a -> (), update f l a

    /// Modifies the state in a state monad and returns the original value.
    let getAndModifyState l f = 
        State.state {
            let! v = State.getState
            do! updateState l f
            return v
        }

    let modifyAndGetState l f = 
        State.state {
            do! updateState l f
            return! State.getState
        }

    let fst =
        { Get = Operators.fst
          Set = fun v a -> v, Operators.snd a }

    let snd =
        { Get = Operators.snd
          Set = fun v a -> Operators.fst a, v }

    let id = 
        { Get = Operators.id
          Set = fun a b -> a }

    let codiag<'a> : Lens<Choice<'a,'a>,'a> = choice id id

    let forSet value =
        { Get = Set.contains value
          Set = fun contains -> (if contains then Set.add else Set.remove) value }

    let forMap key = 
        { Get = Map.tryFind key
          Set = 
            function
            | Some value -> Map.add key value
            | None -> Map.remove key }

    let forArray i = 
        { Get = Array.nth i
          Set = fun v -> Array.copy >> Array.setAt i v }

// not side-effect free
//    let forRef =
//        { Get = (!)
//          Set = fun v a -> a := v; a }

    let ignore = 
        { Get = ignore
          Set = fun _ v -> v }

    module Operators = 
        let inline (.*.) l1 l2 = compose l2 l1
        let inline (.|.) l1 l2 = choice l2 l1
        let inline (+=) l v = update ((+) v) l
        let inline (-=) l v = update ((-) v) l
        let inline (/=) l v = update ((/) v) l
        let inline ( *=) l v = update (( *) v) l
        let inline (|||=) l v = update ((|||) v) l
        let inline (||=) l v = update ((||) v) l
        let inline (&&&=) l v = update ((&&&) v) l
        let inline (&&=) l v = update ((&&) v) l
        let inline (=!) l v = fun a -> set v a l

    module StateOperators = 
        let inline (+=) l v = updateState l ((+) v)
        let inline (-=) l v = updateState l ((-) v)
        let inline (/=) l v = updateState l ((/) v)
        let inline ( *=) l v = updateState l (( *) v)
        let inline (|||=) l v = updateState l ((|||) v)
        let inline (||=) l v = updateState l ((||) v)
        let inline (&&&=) l v = updateState l ((&&&) v)
        let inline (&&=) l v = updateState l ((&&) v)
        let inline (=!) l v = setState l v
