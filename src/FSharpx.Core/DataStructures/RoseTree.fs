namespace FSharpx.DataStructures

open System
open System.Linq

/// Multi-way tree, also known as rose tree.
// Ported from http://hackage.haskell.org/packages/archive/containers/latest/doc/html/src/Data-Tree.html
[<CustomEquality; NoComparison>]
type 'a RoseTree = { Root: 'a; Children: 'a RoseForest }
    with 
    interface IEquatable<'a RoseTree> with
        member x.Equals y = 
            let xc = x.Children :> _ seq
            let yc = y.Children :> _ seq
            obj.Equals(x.Root, y.Root) &&
            xc.SequenceEqual yc
            
and 'a RoseForest = 'a RoseTree LazyList

module L = LazyList

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoseTree =
    let inline create root children = { Root = root; Children = children }

    let inline singleton x = create x L.empty

    let rec map f (x: _ RoseTree) = 
        { RoseTree.Root = f x.Root
          Children = L.map (map f) x.Children }

    let rec ap f x =
        { RoseTree.Root = f.Root x.Root
          Children = 
            let a = L.map (map f.Root) x.Children
            let b = L.map (fun c -> ap c x) f.Children
            L.append a b }

    let rec bind f x =
        let a = f x.Root
        { RoseTree.Root = a.Root
          Children = L.append a.Children (L.map (bind f) x.Children) }

    let rec dfsPre (x: _ RoseTree) =
        seq {
            yield x.Root
            yield! Seq.collect dfsPre x.Children
        }

    let rec dfsPost (x: _ RoseTree) =
        seq {
            yield! Seq.collect dfsPost x.Children
            yield x.Root
        }

    let rec unfold f seed =
        let root, bs = f seed
        create root (unfoldForest f bs)

    and unfoldForest f =
        L.map (unfold f)

// TODO: 
// sequence / mapM / filterM
// zipper: http://hackage.haskell.org/package/rosezipper-0.2