module FSharpx.Collections.Experimental.Tests.BinaryRoseTreeTest

open FSharpx
open FSharpx.Collections.Experimental
open FSharpx.Collections
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FSharpx.Tests.Properties
open FsUnit

let atree = 
    BinaryRoseTree.createTree 1
        (BinaryRoseTree.createTree 2
            (BinaryRoseTree.createForest 3 
                BinaryRoseTree.empty
                (BinaryRoseTree.createTree 4
                    (BinaryRoseTree.createTree 5
                        (BinaryRoseTree.singleton 6)
                    )
                )
            )
        )

let expected = 
    BinaryRoseTree.createTree 2
        (BinaryRoseTree.createTree 3
            (BinaryRoseTree.createForest 4 
                BinaryRoseTree.empty
                (BinaryRoseTree.createTree 5
                    (BinaryRoseTree.createTree 6
                        (BinaryRoseTree.singleton 7)
                    )
                )
            )
        )

let ctree = 
    BinaryRoseTree.createTree "f"
        (
            BinaryRoseTree.createForest "b" 
                (BinaryRoseTree.createForest "a" 
                    BinaryRoseTree.empty
                    (BinaryRoseTree.createTree "d"
                        (BinaryRoseTree.createForest "c" 
                            BinaryRoseTree.empty
                            (BinaryRoseTree.singleton "e")
                        )
                    )
                )

                (BinaryRoseTree.createTree "g"
                    (BinaryRoseTree.createTree "i"
                        (BinaryRoseTree.singleton "h")
                    )
                )
        )

[<Test>]
let ``preOrder works``() =
    let actual = BinaryRoseTree.preOrder ctree |> Seq.toList
    Assert.AreEqual(["f";"b";"a";"d";"c";"e";"g";"i";"h"], actual)

[<Test>]
let ``postOrder works``() =
    let actual = BinaryRoseTree.postOrder ctree |> Seq.toList
    Assert.AreEqual(["a";"c";"e";"d";"b";"h";"i";"g";"f"], actual)
    
[<Test>]
let map() =
    let actual = BinaryRoseTree.map ((+) 1) atree 
    Assert.AreEqual(expected, actual)

[<Test>]
let ``fold via preOrder``() =
    let actual = BinaryRoseTree.preOrder atree |> Seq.fold (*) 1
    Assert.AreEqual(720, actual)

[<Test>]
let ``fold via monoid``() =
    let actual = BinaryRoseTree.foldMap (Monoid.product()) id atree
    Assert.AreEqual(720, actual)

[<Test>]
let ``count nodes and multiply values and find max value in single pass via monoid``() =
    let m = Monoid.tuple3 (Monoid.product()) (Monoid.sum()) Monoid.maxInt
    let product, count, maxValue = BinaryRoseTree.foldMap m (fun v -> v, 1, v) atree
    Assert.AreEqual(720, product)
    Assert.AreEqual(6, count)
    Assert.AreEqual(6, maxValue)

let iRT = BinaryRoseTree.createTree 1 (BinaryRoseTree.createForest 2 atree expected)
let singleRT = BinaryRoseTree.singleton 1

[<Test>]
let ``functor laws``() =
    //fsCheck version of functor and monad laws stackoverflows 
    let map = BinaryRoseTree.map
    
    //preserves identity
    ((map id iRT) = iRT) |> should equal true
    ((map id singleRT) = singleRT) |> should equal true
    
    let f = (fun x -> x + 5)
    let g = (fun x -> x - 2)

    //preserves composition
    map (f << g) iRT = (map f << map g) iRT |> should equal true
    map (f << g) singleRT = (map f << map g) singleRT |> should equal true