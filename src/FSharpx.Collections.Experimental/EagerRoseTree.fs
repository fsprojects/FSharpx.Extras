namespace FSharpx.Collections.Experimental

open FSharpx.Collections
open System
open System.Linq
open System.Runtime.CompilerServices

/// Multi-way tree, also known as rose tree.
/// This RoseTree uses a List for the children RoseTree forest.
/// Adapted from @mausch F# adaptation of Experimental.RoseTree.
// Ported from http://hackage.haskell.org/packages/archive/containers/latest/doc/html/src/Data-Tree.html
[<CustomEquality; NoComparison>]
type EagerRoseTree<'T> = 
    { Root: 'T; Children: EagerRoseForest<'T> }
    override x.Equals y = 
        match y with
        | :? EagerRoseTree<'T> as y ->
            (x :> _ IEquatable).Equals y
        | _ -> false

    override x.GetHashCode() = 
        391
        + (box x.Root).GetHashCode() * 23
        + x.Children.GetHashCode()
    interface IEquatable<EagerRoseTree<'T>> with

        member x.Equals y = 
            obj.Equals(x.Root, y.Root) && (x.Children :> _ seq).SequenceEqual y.Children
            
and EagerRoseForest<'T> = EagerRoseTree<'T> list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<Extension>]
module EagerRoseTree =
    open FSharpx

    let inline create root children = { Root = root; Children = children }

    let inline singleton x = create x List.empty

    let rec map f (x: _ EagerRoseTree) = 
        { EagerRoseTree.Root = f x.Root
          Children = List.map (map f) x.Children }

    let rec ap x f =
        { EagerRoseTree.Root = f.Root x.Root
          Children = 
            let a = List.map (map f.Root) x.Children
            let b = List.map (fun c -> ap x c) f.Children
            List.append a b }

    let inline lift2 f a b = singleton f |> ap a |> ap b

    let rec bind f x =
        let a = f x.Root
        { EagerRoseTree.Root = a.Root
          Children = List.append a.Children (List.map (bind f) x.Children) }

    [<CompiledName("DfsPre")>]
    [<Extension>]
    let rec dfsPre (x: _ EagerRoseTree) =
        seq {
            yield x.Root
            yield! Seq.collect dfsPre x.Children
        }

    [<CompiledName("DfsPost")>]
    [<Extension>]
    let rec dfsPost (x: _ EagerRoseTree) =
        seq {
            yield! Seq.collect dfsPost x.Children
            yield x.Root
        }

    let rec unfold f seed =
        let root, bs = f seed
        create root (unfoldForest f bs)

    and unfoldForest f =
        List.map (unfold f)

    let rec foldMap (monoid: _ Monoid) f (tree: _ EagerRoseTree) =
        let inline (++) a b = monoid.Combine(a,b)
        f tree.Root ++ Seq.foldMap monoid (foldMap monoid f) tree.Children

    /// Behaves like a combination of map and fold; 
    /// it applies a function to each element of a tree, 
    /// passing an accumulating parameter, 
    /// and returning a final value of this accumulator together with the new tree.
    let rec mapAccum f state tree =
        let nstate, root = f state tree.Root
        let nstate, children = List.mapAccum (mapAccum f) nstate tree.Children
        nstate, create root children
        
// TODO: 
// bfs: http://pdf.aminer.org/000/309/950/the_under_appreciated_unfold.pdf
// sequence / mapM / filterM
// zipper: http://hackage.haskell.org/package/rosezipper-0.2
