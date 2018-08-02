namespace FSharpx

module Validation =
    open FSharpx.Collections
    open Choice
    open Monoid

    let (|Success|Failure|) = 
        function
        | Choice1Of2 a -> Success a
        | Choice2Of2 e -> Failure e

    /// Sequential application, parameterized by append
    let apa append x f = 
        match f,x with
        | Choice1Of2 f, Choice1Of2 x     -> Choice1Of2 (f x)
        | Choice2Of2 e, Choice1Of2 x     -> Choice2Of2 e
        | Choice1Of2 f, Choice2Of2 e     -> Choice2Of2 e
        | Choice2Of2 e1, Choice2Of2 e2 -> Choice2Of2 (append e1 e2)

    /// Sequential application, parameterized by semigroup
    let inline apm (m: _ ISemigroup) = apa (curry m.Combine)

    type CustomValidation<'T>(semigroup: 'T ISemigroup) =
        /// Sequential application
        member this.ap x = apm semigroup x

        /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
        member this.lift2 f a b = returnM f |> this.ap a |> this.ap b

        /// Sequence actions, discarding the value of the first argument.
        member this.apr b a = this.lift2 (fun _ z -> z) a b

        /// Sequence actions, discarding the value of the second argument.
        member this.apl b a = this.lift2 (fun z _ -> z) a b

        member this.seqValidator f = 
            let inline cons a b = this.lift2 (flip List.cons) a b
            Seq.map f >> Seq.fold cons (returnM [])

        member this.sequence s =
            let inline cons a b = this.lift2 List.cons a b
            List.foldBack cons s (returnM [])

        member this.mapM f x = this.sequence (List.map f x)


    type NonEmptyListSemigroup<'T>() = 
        interface ISemigroup<'T NonEmptyList> with 
            member x.Combine(a,b) = NonEmptyList.append a b 

    type NonEmptyListValidation<'T>() = 
        inherit CustomValidation<'T NonEmptyList>(NonEmptyListSemigroup<'T>())

    /// Sequential application
    let inline ap x = apa NonEmptyList.append x

    /// Sequential application
    let inline (<*>) f x = ap x f

    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f a b = returnM f <*> a <*> b

    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = lift2 (fun _ z -> z) x y

    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = lift2 (fun z _ -> z) x y

    let seqValidator f = 
        let inline cons a b = lift2 (flip List.cons) a b
        Seq.map f >> Seq.fold cons (returnM [])

    let inline sequence s =
        let inline cons a b = lift2 List.cons a b
        List.foldBack cons s (returnM [])

    let inline mapM f x = sequence (List.map f x)