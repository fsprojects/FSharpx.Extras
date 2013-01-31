module FSharpx.Collections.Experimental.Tests.EagerRoseTreeTest

open FSharpx
open FSharpx.Collections.Experimental
open FSharpx.Collections
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FSharpx.Tests.Properties

let tree a b = EagerRoseTree.create a (Vector.ofSeq b)

let atree = tree 1 [tree 2 [tree 3 []]; tree 4 [tree 5 [tree 6 []]]]
let ctree = tree "f" [tree "b" [tree "a" []; tree "d" [tree "c" []; tree "e" []]]; tree "g" [tree "i" [tree "h" []]]]

[<Test>]
let ``preOrder works``() =
    let actual = EagerRoseTree.preOrder ctree |> Seq.toList
    Assert.AreEqual(["f";"b";"a";"d";"c";"e";"g";"i";"h"], actual)

[<Test>]
let ``postOrder works``() =
    let actual = EagerRoseTree.postOrder ctree |> Seq.toList
    Assert.AreEqual(["a";"c";"e";"d";"b";"h";"i";"g";"f"], actual)
    
[<Test>]
let map() =
    let actual = EagerRoseTree.map ((+) 1) atree
    let expected = tree 2 [tree 3 [tree 4 []]; tree 5 [tree 6 [tree 7 []]]]
    Assert.AreEqual(expected, actual)

[<Test>]
let ``fold via preOrder``() =
    let actual = EagerRoseTree.preOrder atree |> Seq.fold (*) 1
    Assert.AreEqual(720, actual)

[<Test>]
let ``fold via monoid``() =
    let actual = EagerRoseTree.foldMap (Monoid.product()) id atree
    Assert.AreEqual(720, actual)

[<Test>]
let ``count nodes and multiply values and find max value in single pass via monoid``() =
    let m = Monoid.tuple3 (Monoid.product()) (Monoid.sum()) Monoid.maxInt
    let product, count, maxValue = EagerRoseTree.foldMap m (fun v -> v, 1, v) atree
    Assert.AreEqual(720, product)
    Assert.AreEqual(6, count)
    Assert.AreEqual(6, maxValue)

[<Test>]
let unfold() =
    let a = EagerRoseTree.unfold (fun i -> i, Vector.ofSeq {i+1..3}) 0
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
type Html = HtmlNode EagerRoseTree

let htmldoc = 
    tree (elem "body") [tree (elem "div") [text "hello world"]]

[<Test>]
let bind() =
    let wrapText =
        function
        | Text t -> tree (elem "span") [text t]
        | x -> EagerRoseTree.singleton x
    let newDoc = htmldoc |> EagerRoseTree.bind wrapText
    let expected = tree (elem "body") [tree (elem "div") [tree (elem "span") [text "hello world"]]]
    Assert.AreEqual(expected, newDoc)
    
type EagerRoseTreeGen =
    static member EagerRoseTree() =
        let rec eagerRoseTreeGen() = 
            gen {
                let! root = Arb.generate<int>
                let! n = Gen.length1thru100 
                let! l = Gen.listInt n
                return EagerRoseTree.create root (Vector.ofSeq (List.fold (fun (s : list<EagerRoseTree<int>>) (t : int) -> (EagerRoseTree.singleton t)::s) [] l))
            }
        Arb.fromGen (eagerRoseTreeGen())

let registerGen = lazy (Arb.register<EagerRoseTreeGen>() |> ignore)

[<Test>]
let equality() =
    registerGen.Force()
    checkEquality<int EagerRoseTree> "EagerRoseTree"