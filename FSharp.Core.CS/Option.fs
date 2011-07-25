namespace FSharp.Core.CS

open System
open System.Runtime.CompilerServices

[<Extension>]
type FSharpOptionExtensions =
    [<Extension>]
    static member HasValue o = Option.isSome o

    [<Extension>]
    static member ToOption (n: Nullable<_>) =
        if n.HasValue
            then Some n.Value
            else None

    [<Extension>]
    static member Some a = Option.Some a

    [<Extension>]
    static member Match (o, ifSome: Func<_,_>, ifNone: Func<_>) =
        match o with
        | Some x -> ifSome.Invoke x
        | _ -> ifNone.Invoke()

    // LINQ
    [<Extension>]
    static member Select (o, f: Func<_,_>) = Option.map f.Invoke o

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>) = Option.bind f.Invoke o

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>, mapper: Func<_,_,_>) =
        let c = Option.bind f.Invoke o
        match o,c with
        | Some a,Some b -> mapper.Invoke(a,b) |> Some
        | _ -> None

    [<Extension>]
    static member Aggregate (o, state, f: Func<_,_,_>) =
        Option.fold (fun s x -> f.Invoke(s,x)) state o

type FSharpOption =
    static member Some a = Option.Some a
    static member SomeUnit = Some()