namespace System.Collections.Generic

open System
open System.Runtime.CompilerServices

[<Extension>]
type DictionaryExtensions =
    [<Extension>]
    static member TryFind (d: IDictionary<_,_>, key) =
        match d.TryGetValue key with
        | true,v -> Some v
        | _ -> None