namespace FSharp.Core.CS

open System
open System.Runtime.CompilerServices

[<Extension>]
module FSharpOptionExtensions =
    [<Extension>]
    let HasValue o = Option.isSome o

    [<Extension>]
    let ToOption (n: Nullable<'a>) =
        if n.HasValue
            then Some n.Value
            else None