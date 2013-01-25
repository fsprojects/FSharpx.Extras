module FSharpx.Collections.Experimental.Tests.ListZipperTest

open FSharpx
open FSharpx.Collections.Experimental
open FSharpx.Collections.Experimental.ListZipper
open NUnit.Framework
   
let chars = ['a'..'z']
let digits = ['0'..'9']

[<Test>]
let ``Can move forward``() =
    let z = chars |> zipper |> forward |> forward
    Assert.AreEqual(focus z,'c')

[<Test>]
let ``Can move back``() =
    let z = chars |> zipper |> forward |> forward |> back
    Assert.AreEqual(focus z,'b')

[<Test>]
let ``Can move to the front``() =
    let z = chars |> zipper |> forward |> forward |> front
    Assert.AreEqual(focus z,'a')

[<Test>]
let ``Can modify an element``() =
    let z = chars |> zipper |> forward |> forward |> modify 'e' |> back |> forward
    Assert.AreEqual(focus z,'e')