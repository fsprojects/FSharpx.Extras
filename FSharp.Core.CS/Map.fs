namespace Microsoft.FSharp.Collections

open System
open System.Runtime.CompilerServices
open Microsoft.FSharp.Collections

[<Extension>]
type FSharpMap =
    static member New([<ParamArray>] values) =
        Map.ofArray values

    [<Extension>]
    static member ToFSharpMap values = Map.ofSeq values