module FSharp.Monad.Enumeratee

open Iteratee

/// An Enumeratee is an Enumerator that feeds data streams to an internal iteratee.
type Enumeratee<'elo,'eli,'acc> = Iteratee<'eli,'acc> -> Iteratee<'elo, Iteratee<'eli,'acc>>

// TODO: EnumerateeBuilder
