[<AutoOpen>]
module FSharpx.DataStructures.Interfaces

type IVector<'a> =
    abstract member Conj : 'a -> IVector<'a>
    abstract member Count : unit -> int
    abstract member AssocN : int*'a -> IVector<'a>