module FSharpx.DataStructures.Tests.TreeZipperTest


open System
open FSharpx.DataStructures
open FSharpx.DataStructures.TreeZipper
open NUnit.Framework
open FsUnit


let tree  = Branch("a", Branch("b", Leaf, Branch("c", Leaf, Leaf)), Branch("d", Leaf, Leaf))

[<Test>]
let ``Can create zipper from tree``() =       
   let z1 = tree |> fromTree
   Assert.AreEqual(z1.focus,tree)

[<Test>]
let ``Can move down to the left inside the zipper``() =       
   let z1 = tree |> fromTree |> move [Left]
   Assert.AreEqual(z1.focus,Branch("b", Leaf, Branch("c", Leaf, Leaf)))

[<Test>]
let ``Can move down to the right inside the zipper``() =       
   let z1 = tree |> fromTree |> move [Right]
   Assert.AreEqual(z1.focus,Branch("d", Leaf, Leaf))

[<Test>]
let ``Can move down to the left and the right inside the zipper``() =       
   let z1 = tree |> fromTree |> move [Left;Right]
   Assert.AreEqual(z1.focus,Branch("c", Leaf, Leaf))

[<Test>]
let ``Can move up inside the zipper``() =       
   let z1 = tree |> fromTree |> move [Left;Right;Right;Up;Up;Up]
   Assert.AreEqual(z1.focus,tree)

[<Test>]
let ``Can move to the top from inside the zipper``() =       
   let z1 = tree |> fromTree |> move [Left;Right;Right] |> top
   Assert.AreEqual(z1.focus,tree)