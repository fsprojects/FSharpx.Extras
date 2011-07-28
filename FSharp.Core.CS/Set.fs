namespace Microsoft.FSharp.Core

open System
open System.Runtime.CompilerServices

[<Extension>]
type FSharpSet =
    static member New([<ParamArray>] values: 'a array) =
        set values

    [<Extension>]
    static member ToFSharpSet values = set values