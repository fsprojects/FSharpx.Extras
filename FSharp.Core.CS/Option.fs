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

    [<Extension>]
    let Some a = Option.Some a

    // LINQ
    [<Extension>]
    let Select (o: 'a option, f: Func<'a, 'b>) =
        match o with
        | Some x -> f.Invoke x |> Some
        | _ -> None

type FSharpOption =
    static member Some a = Option.Some a