namespace FSharpx.DataStructures

open System
open System.Linq

/// Multi-way tree, also known as rose tree.
// Ported from http://hackage.haskell.org/packages/archive/containers/latest/doc/html/src/Data-Tree.html
type 'a RoseTree = { Root: 'a; Children: 'a RoseTree list }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoseTree =
    let inline create root children = { Root = root; Children = children }

    let inline singleton x = create x []

    let rec map f (x: _ RoseTree) = 
        { RoseTree.Root = f x.Root
          Children = List.map (map f) x.Children }

    let rec ap f x =
        { RoseTree.Root = f.Root x.Root
          Children = 
            let a = List.map (map f.Root) x.Children
            let b = List.map (fun c -> ap c x) f.Children
            List.append a b }

    let rec bind f x =
        let a = f x.Root
        { RoseTree.Root = a.Root
          Children = List.append a.Children (List.map (bind f) x.Children) }

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
        List.map (unfold f)

// TODO: 
// sequence / mapM / filterM
// zipper: http://hackage.haskell.org/package/rosezipper-0.2
// Using a LazyList kills equality. Is it worth it to define a custom equality?