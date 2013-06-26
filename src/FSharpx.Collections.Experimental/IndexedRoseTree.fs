namespace FSharpx.Collections.Experimental

open FSharpx
open FSharpx.Collections
open System
open System.Linq
open System.Runtime.CompilerServices

/// Multi-way tree, also known as rose tree.
/// This RoseTree uses a Vector for the children RoseTree forest.
/// Adapted from @mausch F# adaptation of Experimental.RoseTree.
// Ported from http://hackage.haskell.org/packages/archive/containers/latest/doc/html/src/Data-Tree.html
[<CustomEquality; NoComparison>]
type IndexedRoseTree<'T> = { Root: 'T; Children: Vector<IndexedRoseTree<'T>> }
    with
    override x.Equals y = 
        match y with
        | :? IndexedRoseTree<'T> as y ->
            (x :> IEquatable<_>).Equals y
        | _ -> false

    override x.GetHashCode() = 
        391
        + (box x.Root).GetHashCode() * 23
        + x.Children.GetHashCode()

    interface IEquatable<IndexedRoseTree<'T>> with
        member x.Equals y = 
            obj.Equals(x.Root, y.Root) && (x.Children :> _ seq).SequenceEqual y.Children       

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IndexedRoseTree =

    let inline create root children = { Root = root; Children = children }

    let inline singleton x = create x Vector.empty

    let rec map f (x: _ IndexedRoseTree) = 
        { IndexedRoseTree.Root = f x.Root
          Children = Vector.map (map f) x.Children }

    let rec ap x f =
        { IndexedRoseTree.Root = f.Root x.Root
          Children = 
            let a = Vector.map (map f.Root) x.Children
            let b = Vector.map (fun c -> ap x c) f.Children
            Vector.append a b }

    let inline lift2 f a b = singleton f |> ap a |> ap b

    let rec bind f x =
        let a = f x.Root
        { IndexedRoseTree.Root = a.Root
          Children = Vector.append a.Children (Vector.map (bind f) x.Children) }

    let rec preOrder (x: _ IndexedRoseTree) =
        seq {
            yield x.Root
            yield! Seq.collect preOrder x.Children
        }

    let rec postOrder (x: _ IndexedRoseTree) =
        seq {
            yield! Seq.collect postOrder x.Children
            yield x.Root
        }

    let rec unfold f seed =
        let root, bs = f seed
        create root (unfoldForest f bs)

    and unfoldForest f =
        Vector.map (unfold f)

    let rec foldMap (monoid: _ Monoid) f (tree: _ IndexedRoseTree) =
        let inline (++) a b = monoid.Combine(a,b)
        f tree.Root ++ Seq.foldMap monoid (foldMap monoid f) tree.Children
