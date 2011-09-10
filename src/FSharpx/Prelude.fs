namespace FSharpx

[<AutoOpen>]
module Prelude =
  let inline flip f a b = f b a
  let inline curry f a b = f(a,b)
  let inline uncurry f (a,b) = f a b
  let inline swap (a,b) = (b,a)
  let inline konst a _ = a
  let inline konst2 a _ _ = a

  /// Creates a pair
  let inline tuple2 a b = a,b
  /// Creates a 3-tuple
  let inline tuple3 a b c = a,b,c
  /// Creates a 4-tuple
  let inline tuple4 a b c d = a,b,c,d
  /// Creates a 5-tuple
  let inline tuple5 a b c d e = a,b,c,d,e
  /// Creates a 6-tuple
  let inline tuple6 a b c d e f = a,b,c,d,e,f
