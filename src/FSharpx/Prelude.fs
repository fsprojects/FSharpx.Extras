namespace FSharpx

[<AutoOpen>]
module Prelude =
  let inline flip f a b = f b a
  let inline curry f a b = f(a,b)
  let inline uncurry f (a,b) = f a b
  let inline swap (a,b) = (b,a)
  let inline konst a _ = a
  let inline konst2 a _ _ = a


/// Functions to create tuples
module Tuples =
  /// Creates a pair
  let inline t2 a b = a,b
  /// Creates a 3-tuple
  let inline t3 a b c = a,b,c
  /// Creates a 4-tuple
  let inline t4 a b c d = a,b,c,d
  /// Creates a 5-tuple
  let inline t5 a b c d e = a,b,c,d,e
  /// Creates a 6-tuple
  let inline t6 a b c d e f = a,b,c,d,e,f

  let inline fst3 (a,_,_) = a
  let inline snd3 (_,a,_) = a
  let inline thr3 (_,_,a) = a
