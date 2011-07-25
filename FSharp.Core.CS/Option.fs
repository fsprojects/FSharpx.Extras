namespace FSharp.Core.CS

open System.Runtime.CompilerServices

[<Extension>]
module FSharpOptionExtensions =
    [<Extension>]
    let HasValue o = Option.isSome o