namespace FSharp.Monad
open System
open System.Collections.Generic

/// The monoid.
/// <remarks>
/// The monoid implementation comes from Matthew Podwysocki's <see href="http://codebetter.com/blogs/matthew.podwysocki/archive/2010/02/01/a-kick-in-the-monads-writer-edition.aspx"/>.
/// </remarks>
type IMonoid<'a> =
  abstract member mempty  : unit -> 'a
  abstract member mappend : 'a * 'a -> 'a

type ListMonoid<'a>() =
  interface IMonoid<'a list> with
    member this.mempty() = []
    member this.mappend(a,b) = a @ b

type MonoidAssociations private() =
  static let associations = new Dictionary<Type, obj>()
  static member Add<'a>(monoid : IMonoid<'a>) = associations.Add(typeof<'a>, monoid)
  static member Get<'a>() =
    match associations.TryGetValue(typeof<'a>) with
    | true, assoc -> assoc :?> IMonoid<'a>
    | false, _    -> failwithf "No IMonoid defined for %O" <| typeof<'a>

module Monoid =
  MonoidAssociations.Add(new ListMonoid<string>())
  let mempty<'a> = MonoidAssociations.Get<'a>().mempty
  let mappend<'a> a b = MonoidAssociations.Get<'a>().mappend(a, b)