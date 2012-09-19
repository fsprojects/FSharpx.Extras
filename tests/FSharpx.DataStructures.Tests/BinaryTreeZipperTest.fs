module FSharpx.DataStructures.Tests.BinaryTreeZipperTest

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.BinaryTreeZipper
open NUnit.Framework

let tree  = Branch("a", Branch("b", Leaf, Branch("c", Leaf, Leaf)), Branch("d", Leaf, Leaf))

[<Test>]
let ``Can create zipper from tree``() =       
   let z1 = tree |> zipper
   Assert.AreEqual(z1.Focus,tree)

[<Test>]
let ``Can move down to the left inside the zipper``() =       
   let z1 = tree |> zipper |> left
   Assert.AreEqual(z1.Focus,Branch("b", Leaf, Branch("c", Leaf, Leaf)))

[<Test>]
let ``Can move down to the right inside the zipper``() =       
   let z1 = tree |> zipper |> right
   Assert.AreEqual(z1.Focus,Branch("d", Leaf, Leaf))

[<Test>]
let ``Can move down to the left and the right inside the zipper``() =       
   let z1 = tree |> zipper |> move [Left;Right]
   Assert.AreEqual(z1.Focus,Branch("c", Leaf, Leaf))

[<Test>]
let ``Can move up inside the zipper``() =       
   let z1 = tree |> zipper |> move [Left;Right;Right;Up;Up;Up]
   Assert.AreEqual(z1.Focus,tree)

[<Test>]
let ``Can move to the top from inside the zipper``() =       
   let z1 = tree |> zipper |> move [Left;Right;Right] |> top
   Assert.AreEqual(z1.Focus,tree)

[<Test>]
let ``Can modify inside the zipper``() =
   let z1 = tree |> zipper |> right |> setFocus (branch "e") |> top

   Assert.AreEqual(z1.Focus,Branch("a", Branch("b", Leaf, Branch("c", Leaf, Leaf)), Branch("e", Leaf, Leaf)))