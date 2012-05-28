[<AutoOpen>]
module FSharpx.DataStructures.Interfaces

type IVector<'a> =
   abstract member Conj : 'a -> IVector<'a>