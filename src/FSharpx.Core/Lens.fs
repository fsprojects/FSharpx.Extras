namespace FSharpx

open FSharpx.Collections

type Lens<'T,'U> = 
    { Get: 'T -> 'U
      Set: 'U -> 'T -> 'T } 

    member l.Update f a = l.Set (f (l.Get a)) a

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =
    let inline get a (l: Lens<_,_>) = l.Get a
    let inline set v a (l: Lens<_,_>) = l.Set v a
    let inline update f (l: Lens<_,_>) = l.Update f

    /// Sequentially composes two lenses
    let inline compose (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        { Get = l2.Get >> l1.Get
          Set = l1.Set >> l2.Update }

    /// Composes two lenses through a sum in the source
    let inline choice (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        { Get = Choice.choice l1.Get l2.Get
          Set = fun b -> Choice.bimap (l1.Set b) (l2.Set b) }

    /// Pair two lenses
    let inline pair (l1: Lens<_,_>) (l2: Lens<_,_>) = 
        { Get = fun (a,b) -> (l1.Get a, l2.Get b)
          Set = fun (a,c) (b,d) -> (l1.Set a b, l2.Set c d) }

    /// <summary>
    /// <paramref name="pred"/> is applied to source. 
    /// If true, <paramref name="lensTrue"/> is selected.
    /// If false, <paramref name="lensFalse"/> is selected.
    /// </summary>
    let cond pred lensTrue lensFalse =
        let inline choose a = if pred a then lensTrue else lensFalse
        { Get = fun a -> choose a |> get a
          Set = fun b a -> choose a |> set b a }

    /// Applies a lens in the 'get' direction within a state monad      
    let getState l = 
        fun a -> get a l, a

    /// Applies a lens in the 'set' direction within a state monad
    let setState l v = 
        fun a -> (), set v a l

    /// Update through a lens within a state monad
    let updateState l f =
        fun a -> (), update f l a

    /// Modifies the state in a state monad and returns the original value.
    let getAndModifyState l f = 
        State.state {
            let! v = State.getState
            do! updateState l f
            return v
        }

    /// Modifies the state in a state monad and returns the modified value.
    let modifyAndGetState l f = 
        State.state {
            do! updateState l f
            return! State.getState
        }

    /// Gets/sets the fst element in a pair
    let fst =
        { Get = Operators.fst
          Set = fun v a -> v, Operators.snd a }

    /// Gets/sets the snd element in a pair
    let snd =
        { Get = Operators.snd
          Set = fun v a -> Operators.fst a, v }

    /// Identity lens
    let id = 
        { Get = Operators.id
          Set = fun a b -> a }

    let codiag<'T> : Lens<Choice<'T,'T>,'T> = choice id id

    /// Lens for a particular value in a set
    let forSet value =
        { Get = Set.contains value
          Set = fun contains -> (if contains then Set.add else Set.remove) value }

    /// Lens for a particular key in a map
    let forMap key = 
        { Get = Map.tryFind key
          Set = 
            function
            | Some value -> Map.add key value
            | None -> Map.remove key }

    /// Lens for a particular position in an array
    let forArray i = 
        { Get = Array.nth i
          Set = fun v -> Array.copy >> Array.setAt i v }

    /// Lens for a particular position in a list
    let forList i =
        { Get = (flip List.nth) i
          Set = fun v -> List.mapi (fun j e -> if j = i then v else e) }

    /// Creates a lens that maps the given lens in a list
    let listMap (l: Lens<_,_>) =
        { Get = List.map l.Get
          Set = List.map2 l.Set }

    /// Creates a lens that maps the given lens in an array
    let arrayMap (l: Lens<_,_>) = 
        { Get = Array.map l.Get
          Set = Array.map2 l.Set }

    /// Creates a lens that maps the given lens in a sequence
    let seqMap (l: Lens<_,_>) = 
        { Get = Seq.map l.Get
          Set = Seq.map2 l.Set }

    /// Applies an isomorphism to the value viewed through a lens
    let xmap f g (l: Lens<_,_>) = 
        { Get = l.Get >> f
          Set = g >> l.Set }

    /// Converts a lens that views a list into a lens that views an array
    let inline listToArray l = xmap List.toArray Array.toList l

// not side-effect free
//    let forRef =
//        { Get = (!)
//          Set = fun v a -> a := v; a }

    let ignore = 
        { Get = ignore
          Set = fun _ v -> v }

    module Operators = 
        let inline (>>|) l1 l2 = compose l2 l1
        let inline (|<<) l1 l2 = compose l1 l2
        let inline (.|.) l1 l2 = choice l2 l1
        let inline ( ***) l1 l2 = pair l2 l1
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
