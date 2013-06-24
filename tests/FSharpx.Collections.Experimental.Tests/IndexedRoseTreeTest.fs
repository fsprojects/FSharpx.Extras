module FSharpx.Collections.Experimental.Tests.IndexedRoseTreeTest

open FSharpx
open FSharpx.Collections.Experimental
open FSharpx.Collections
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FSharpx.Tests.Properties
open FsUnit

let tree a b = IndexedRoseTree.create a (Vector.ofSeq b)

let atree = tree 1 [tree 2 [tree 3 []]; tree 4 [tree 5 [tree 6 []]]]
let ctree = tree "f" [tree "b" [tree "a" []; tree "d" [tree "c" []; tree "e" []]]; tree "g" [tree "i" [tree "h" []]]]

[<Test>]
let ``preOrder works``() =
    let actual = IndexedRoseTree.preOrder ctree |> Seq.toList
    Assert.AreEqual(["f";"b";"a";"d";"c";"e";"g";"i";"h"], actual)

[<Test>]
let ``postOrder works``() =
    let actual = IndexedRoseTree.postOrder ctree |> Seq.toList
    Assert.AreEqual(["a";"c";"e";"d";"b";"h";"i";"g";"f"], actual)
    
[<Test>]
let map() =
    let actual = IndexedRoseTree.map ((+) 1) atree
    let expected = tree 2 [tree 3 [tree 4 []]; tree 5 [tree 6 [tree 7 []]]]
    Assert.AreEqual(expected, actual)

[<Test>]
let ``fold via preOrder``() =
    let actual = IndexedRoseTree.preOrder atree |> Seq.fold (*) 1
    Assert.AreEqual(720, actual)

[<Test>]
let ``fold via monoid``() =
    let actual = IndexedRoseTree.foldMap (Monoid.product()) id atree
    Assert.AreEqual(720, actual)

[<Test>]
let ``count nodes and multiply values and find max value in single pass via monoid``() =
    let m = Monoid.tuple3 (Monoid.product()) (Monoid.sum()) Monoid.maxInt
    let product, count, maxValue = IndexedRoseTree.foldMap m (fun v -> v, 1, v) atree
    Assert.AreEqual(720, product)
    Assert.AreEqual(6, count)
    Assert.AreEqual(6, maxValue)

[<Test>]
let unfold() =
    let a = IndexedRoseTree.unfold (fun i -> i, Vector.ofSeq {i+1..3}) 0
    let expected = tree 0 [tree 1 [tree 2 [tree 3 []]; tree 3 []]; tree 2 [tree 3 []]; tree 3 []]
    Assert.AreEqual(expected, a)

// not the best example, as text nodes cannot have children

type HtmlElement = { TagName: string; Attributes: (string * string) list }

type HtmlNode =
| Element of HtmlElement
| Text of string

let elemA tag attr = HtmlNode.Element { TagName = tag; Attributes = attr }
let elem tag = elemA tag []
let text t = tree (HtmlNode.Text t) []
type Html = HtmlNode IndexedRoseTree

let htmldoc = 
    tree (elem "body") [tree (elem "div") [text "hello world"]]

[<Test>]
let bind() =
    let wrapText =
        function
        | Text t -> tree (elem "span") [text t]
        | x -> IndexedRoseTree.singleton x
    let newDoc = htmldoc |> IndexedRoseTree.bind wrapText
    let expected = tree (elem "body") [tree (elem "div") [tree (elem "span") [text "hello world"]]]
    Assert.AreEqual(expected, newDoc)
    
let finiteIndexedRoseTreeForest() =
    gen {
        let! n = Gen.length1thru 5
        let! l = Gen.listOfLength n Arb.generate<int>
        return Seq.fold (fun (s : list<IndexedRoseTree<int>>) (t : int) -> (IndexedRoseTree.singleton t)::s) [] l |> Vector.ofSeq
    }

type IndexedRoseTreeGen =
    static member IndexedRoseTree() =
        let rec IndexedRoseTreeGen() = 
            gen {
                let! root = Arb.generate<int>
                // need to set these frequencies to avoid blowing the stack
                let! children = Gen.frequency [70, gen.Return Vector.empty; 1, finiteIndexedRoseTreeForest()]
                return IndexedRoseTree.create root children
            }
        Arb.fromGen (IndexedRoseTreeGen())

let registerGen = lazy (Arb.register<IndexedRoseTreeGen>() |> ignore)

[<Test>]
let equality() =
    registerGen.Force()
    checkEquality<int IndexedRoseTree> "IndexedRoseTree"

let iRTF l = List.fold (fun (s : list<IndexedRoseTree<int>>) (t : int) -> (IndexedRoseTree.singleton t)::s) [] l |> Vector.ofSeq
let iRTF2 = 
    let rec loop (v : Vector<IndexedRoseTree<int>>) dec =
        match dec with
        | 0 -> v
        | _ -> loop (v.Conj (IndexedRoseTree.create 1 (iRTF [1..5]))) (dec - 1)
    loop Vector.empty 5


let iRT = IndexedRoseTree.create 1 iRTF2
let singleRT = IndexedRoseTree.singleton 1

[<Test>]
let ``functor laws``() =
    //fsCheck version of functor and monad laws stackoverflows 
    let map = IndexedRoseTree.map
    
    //preserves identity
    ((map id iRT) = iRT) |> should equal true
    ((map id singleRT) = singleRT) |> should equal true
    
    let f = (fun x -> x + 5)
    let g = (fun x -> x - 2)

    //preserves composition
    map (f << g) iRT = (map f << map g) iRT |> should equal true
    map (f << g) singleRT = (map f << map g) singleRT |> should equal true

[<Test>]
let ``monad laws``() =
    //fsCheck version of functor and monad laws stackoverflows
    let inline (>>=) m f = IndexedRoseTree.bind f m
    let ret = IndexedRoseTree.singleton

    let myF x = IndexedRoseTree.create x (Vector.empty |> Vector.conj (IndexedRoseTree.singleton x) |> Vector.conj  (IndexedRoseTree.singleton x))
    let a = 1

    //left identity 
    ret a >>= myF = myF a |> should equal true

    //right identity 
    iRT >>= ret = iRT |> should equal true
    singleRT >>= ret = singleRT |> should equal true

    //associativity 
    let myG x = IndexedRoseTree.create (x=x) (Vector.empty |> Vector.conj (IndexedRoseTree.singleton (x=x)) |> Vector.conj  (IndexedRoseTree.singleton (x=x)))

    let a' = (iRT >>= myF) >>= myG
    let b' = iRT >>= (fun x -> myF x >>= myG)
    a' = b' |> should equal true

    let a'' = (singleRT >>= myF) >>= myG
    let b'' = singleRT >>= (fun x -> myF x >>= myG)
    a'' = b'' |> should equal true