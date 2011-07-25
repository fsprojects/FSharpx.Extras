namespace FSharp.Core.CS

open System
open System.Runtime.CompilerServices

[<Extension>]
type ListExtensions =
    [<Extension>]
    static member Match (l, empty: Func<_>, nonempty: Func<_,_,_>) =
        match l with
        | [] -> empty.Invoke()
        | x::xs -> nonempty.Invoke(x,xs)