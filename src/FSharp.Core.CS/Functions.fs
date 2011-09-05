namespace Microsoft.FSharp.Core

open System
open System.Runtime.CompilerServices

/// <summary>
/// Conversion functions from Action/Func to FSharpFunc
/// We need these because FuncConvert often makes C# type inference fail.
/// </summary>
[<Extension>]
type FSharpFunc =
    static member FromAction (f: Action) =
        fun () -> f.Invoke()
    
    static member FromAction (f: Action<_>) =
        fun x -> f.Invoke x

    static member FromAction (f: Action<_,_>) =
        fun x y -> f.Invoke(x,y)

    static member FromAction (f: Action<_,_,_>) =
        fun x y z -> f.Invoke(x,y,z)

    static member FromFunc (f: Func<_>) =
        fun () -> f.Invoke()

    static member FromFunc (f: Func<_,_>) =
        fun x -> f.Invoke x

    static member FromFunc (f: Func<_,_,_>) =
        fun x y -> f.Invoke(x,y)

    [<Extension>]
    static member Curry (f: Func<_,_,_>) =
        Func<_,Func<_,_>>(fun a -> Func<_,_>(fun b -> f.Invoke(a,b)))