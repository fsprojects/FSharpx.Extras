namespace FSharpx

[<AutoOpen>]
module Prelude =
  let inline flip f a b = f b a
  let inline curry f a b = f(a,b)
  let inline uncurry f (a,b) = f a b
  let inline swap (a,b) = (b,a)
  let inline konst a _ = a
  let inline konst2 a _ _ = a


