module FSharpx.DataStructures.Tests.RoseTreeTest

open FSharpx
open FSharpx.DataStructures
open NUnit.Framework

let tree a b = RoseTree.create a (LazyList.ofList b)

let atree = tree 1 [tree 2 [tree 3 []]; tree 4 [tree 5 [tree 6 []]]]
let ctree = tree "f" [tree "b" [tree "a" []; tree "d" [tree "c" []; tree "e" []]]; tree "g" [tree "i" [tree "h" []]]]

// examples borrowed from http://en.wikipedia.org/wiki/Tree_traversal#Example

[<Test>]
let ``dfs pre``() =
    let actual = RoseTree.dfsPre ctree |> Seq.toList
    Assert.AreEqual(["f";"b";"a";"d";"c";"e";"g";"i";"h"], actual)

[<Test>]
let ``dfs post``() =
    let actual = RoseTree.dfsPost ctree |> Seq.toList
    Assert.AreEqual(["a";"c";"e";"d";"b";"h";"i";"g";"f"], actual)
    
[<Test>]
let map() =
    let actual = RoseTree.map ((+) 1) atree
    let expected = tree 2 [tree 3 [tree 4 []]; tree 5 [tree 6 [tree 7 []]]]
    Assert.AreEqual(expected, actual)

// not the best example, as text nodes cannot have children

type HtmlElement = { TagName: string; Attributes: (string * string) list }

type HtmlNode =
| Element of HtmlElement
| Text of string

let elemA tag attr = HtmlNode.Element { TagName = tag; Attributes = attr }
let elem tag = elemA tag []
let text t = tree (HtmlNode.Text t) []
type Html = HtmlNode RoseTree

let htmldoc = 
    tree (elem "body") [tree (elem "div") [text "hello world"]]

[<Test>]
let bind() =
    let wrapText =
        function
        | Element _ as x -> RoseTree.singleton x
        | Text t -> tree (elem "span") [text t]
    let newDoc = htmldoc |> RoseTree.bind wrapText
    let expected = tree (elem "body") [tree (elem "div") [tree (elem "span") [text "hello world"]]]
    Assert.AreEqual(expected, newDoc)
    printfn "%A" newDoc

// TODO port example from http://blog.moertel.com/articles/2007/03/07/directory-tree-printing-in-haskell-part-two-refactoring
