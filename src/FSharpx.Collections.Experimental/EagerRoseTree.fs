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
type 'a EagerRoseTree = { Root: 'a; Children: 'a EagerRoseTree Vector }
    with
    override x.Equals y = 
        match y with
        | :? EagerRoseTree<'a> as y ->
            (x :> _ IEquatable).Equals y
        | _ -> false
    override x.GetHashCode() = 
        391
        + (box x.Root).GetHashCode() * 23
        + x.Children.GetHashCode()
    interface IEquatable<'a EagerRoseTree> with
        member x.Equals y = 
            obj.Equals(x.Root, y.Root) && (x.Children :> _ seq).SequenceEqual y.Children       

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module EagerRoseTree =

    let inline create root children = { Root = root; Children = children }

    let inline singleton x = create x Vector.empty

    let rec map f (x: _ EagerRoseTree) = 
        { EagerRoseTree.Root = f x.Root
          Children = Vector.map (map f) x.Children }

    let rec ap x f =
        { EagerRoseTree.Root = f.Root x.Root
          Children = 
            let a = Vector.map (map f.Root) x.Children
            let b = Vector.map (fun c -> ap x c) f.Children
            Vector.append a b }

    let inline lift2 f a b = singleton f |> ap a |> ap b

    let rec bind f x =
        let a = f x.Root
        { EagerRoseTree.Root = a.Root
          Children = Vector.append a.Children (Vector.map (bind f) x.Children) }

    let rec preOrder (x: _ EagerRoseTree) =
        seq {
            yield x.Root
            yield! Seq.collect preOrder x.Children
        }

    let rec postOrder (x: _ EagerRoseTree) =
        seq {
            yield! Seq.collect postOrder x.Children
            yield x.Root
        }

    let rec unfold f seed =
        let root, bs = f seed
        create root (unfoldForest f bs)

    and unfoldForest f =
        Vector.map (unfold f)

    let rec foldMap (monoid: _ Monoid) f (tree: _ EagerRoseTree) =
        let inline (++) a b = monoid.Combine(a,b)
        f tree.Root ++ Seq.foldMap monoid (foldMap monoid f) tree.Children
