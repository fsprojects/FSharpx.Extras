namespace FSharp.Core.CS

open System
open System.Runtime.CompilerServices

[<Extension>]
type TaggedUnionExtensions =
    [<Extension>]
    static member Match (c, f1: Func<_,_>, f2: Func<_,_>) =
        match c with
        | Choice1Of2 x -> f1.Invoke x
        | Choice2Of2 y -> f2.Invoke y

