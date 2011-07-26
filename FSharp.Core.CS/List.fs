namespace FSharp.Core.CS

open System
open System.Runtime.CompilerServices

[<Extension>]
type FSharpList =
    [<Extension>]
    static member Match (l, empty: Func<_>, nonempty: Func<_,_,_>) =
        match l with
        | [] -> empty.Invoke()
        | x::xs -> nonempty.Invoke(x,xs)

    [<Extension>]
    static member Choose (l, chooser: Func<_,_>) =
        List.choose chooser.Invoke l

    [<Extension>]
    static member Cons (l, e) = e::l

    static member New([<ParamArray>] values: 'a array) =
        Seq.toList values
