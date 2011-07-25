namespace FSharp.Core.CS

open System
open System.Runtime.CompilerServices

[<Extension>]
type FSharpOptionExtensions =
    [<Extension>]
    static member HasValue o = Option.isSome o

    [<Extension>]
    static member ToOption (n: Nullable<'a>) =
        if n.HasValue
            then Some n.Value
            else None

    [<Extension>]
    static member Some a = Option.Some a

    [<Extension>]
    static member Match (o: 'a option, ifSome: Func<'a, 'b>, ifNone: Func<'b>) =
        match o with
        | Some x -> ifSome.Invoke x
        | _ -> ifNone.Invoke()

    // LINQ
    [<Extension>]
    static member Select (o: 'a option, f: Func<'a, 'b>) = Option.map f.Invoke o

    [<Extension>]
    static member SelectMany (o: 'a option, f: Func<'a, 'b option>) = Option.bind f.Invoke o

    [<Extension>]
    static member SelectMany (o: 'a option, f: Func<'a, 'b option>, mapper: Func<'a,'b,'c>) =
        let c = Option.bind f.Invoke o
        match o,c with
        | Some a,Some b -> mapper.Invoke(a,b) |> Some
        | _ -> None

type FSharpOption =
    static member Some a = Option.Some a
    static member SomeUnit = Some()