namespace FSharpx.Collections.Experimental

open System
open System.Runtime.CompilerServices
open FSharpx
open FSharpx.Collections

[<Extension>]
type RoseTree =
    static member Singleton x = RoseTree.singleton x
    static member Create(root, [<ParamArray>] children) = RoseTree.create root (LazyList.ofArray children)
    static member Create(root, children) = RoseTree.create root children

    [<Extension>]
    static member Select(tree, f: Func<_,_>) = RoseTree.map f.Invoke tree

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>) = RoseTree.bind f.Invoke o

    [<Extension>]
    static member SelectMany (o, f: Func<_,_>, mapper: Func<_,_,_>) =
        let mapper = RoseTree.lift2 (curry mapper.Invoke)
        let v = RoseTree.bind f.Invoke o
        mapper o v

    [<Extension>]
    static member SelectAccum (o, state, f: Func<_,_,_>) =
        let ff = FSharpFunc.FromFunc f
        RoseTree.mapAccum ff state o
