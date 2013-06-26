namespace FSharpx.Collections.Experimental

open FSharpx
open FSharpx.Collections
open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Runtime.CompilerServices

/// Multi-way tree, also known as rose tree.
/// This RoseTree uses a Vector for the children RoseTree forest.
/// Adapted from @mausch F# adaptation of Experimental.RoseTree.
// Ported from http://hackage.haskell.org/packages/archive/containers/latest/doc/html/src/Data-Tree.html
[<CustomEquality; NoComparison>]
type BinaryRoseTree<'T> = //{ Root: 'T; Children: 'T BinaryRoseTree Vector }
    | Nil
    | Node of 'T * BinaryRoseTree<'T> * BinaryRoseTree<'T>
    override x.Equals y = 
        match y with
        | :? BinaryRoseTree<'T> as y ->
            (x :> _ IEquatable).Equals y
        | _ -> false

    override x.GetHashCode() = 
        let mutable hash = 1
        for v in x do hash <- 31 * hash + Unchecked.hash v
        hash

    interface IEquatable<BinaryRoseTree<'T>> with
        member x.Equals y = x.SequenceEqual y      
            
    interface IEnumerable<'T> with
        member x.GetEnumerator() =
            (let rec loop x =
                seq {
                        match x with
                        | Node(a, Nil, Nil) -> yield a
                        | Node(a, children, siblings) -> 
                            yield a
                            yield! loop children
                            yield! loop siblings 
                        | Nil -> ()
                }
            loop x).GetEnumerator()
        
        member x.GetEnumerator() =
            (x :> seq<'T>).GetEnumerator() :> IEnumerator 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BinaryRoseTree =

    let inline createTree root (children : BinaryRoseTree<'T>) = Node(root, children, Nil)
    let inline createForest root (children : BinaryRoseTree<'T>) (sibling : BinaryRoseTree<'T>) = Node(root, children, sibling)

    let empty = Nil
    let inline singleton root = createTree root Nil

    /// loads from sequences of objects and sequences of objects, assumed to be well-constructed
    /// elements in final tree will be objects, regardless of original type
    let ofSeq (xs : seq<obj>) = 
        
        let rec outerLoop (xs' : seq<obj>) =
            let lazyXs = LazyList.ofSeq xs'

            let rec loop (xs'' : LazyList<obj>) =
                match xs'' with
                | LazyList.Nil -> Nil
                | LazyList.Cons(x, LazyList.Nil) -> 
                    match x with
                    | :? seq<obj> as x' -> outerLoop x'
                    | _ -> Node(x, Nil, Nil)

                | LazyList.Cons(x, ys) -> 
                    match x with
                    | :? seq<obj> as x' -> outerLoop x'
                    | _ -> 
                        match ys with
                        | LazyList.Nil -> Node(x, Nil, Nil)
                        | LazyList.Cons(y, ys') -> 
                            match y with
                            | :? seq<obj> as y' -> Node(x, (outerLoop y'), (loop ys'))  
                            | _ -> Node(x, Nil,  (loop ys)) 
                             
            loop lazyXs

        outerLoop xs

    let private map2 nodeF leafV (x: _ BinaryRoseTree) = 

        let rec loop (x: _ BinaryRoseTree) cont =
            match x with
            | Node(a, children, siblings) -> 
                loop children (fun lacc ->  
                    loop siblings (fun racc -> 
                        cont (nodeF a lacc racc))) 
            | Nil -> cont leafV
            
        loop x (fun x -> x)

    let map f (tree : _ BinaryRoseTree) = (map2 (fun x (l : BinaryRoseTree<_>) (r  : BinaryRoseTree<_>) -> Node(f x, l, r)) Nil tree)

    let rec preOrder (x: _ BinaryRoseTree) =
        seq {
                match x with
                | Node(a, Nil, Nil) -> yield a
                | Node(a, children, siblings) -> 
                    yield a
                    yield! preOrder children
                    yield! preOrder siblings 
                | Nil -> ()
        }

    let rec postOrder (x: _ BinaryRoseTree) =
        seq {
                match x with
                | Node(a, Nil, Nil) -> yield a
                | Node(a, children, siblings) -> 
                    yield! postOrder children
                    yield a
                    yield! postOrder siblings
                | Nil -> ()
        }

    let rec foldMap (monoid: _ Monoid) f (tree: _ BinaryRoseTree) =
        Seq.foldMap monoid f tree
