module FSharpx.DataStructures.Tests.LeftistHeapTest

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.LeftistHeap
open NUnit.Framework
open FsUnit


//only going up to len5 is probably sufficient to test all edge cases
//but better too many unit tests than too few

[<Test>]
let ``empty list should be empty``() =
    empty true |> isEmpty |> should equal true

[<Test>]
let ``insert works``() =
    empty false |> insert 1 |> insert 2 |> isEmpty |> should equal false

[<Test>]
let ``uncons 1 element``() =
    let x, _ = empty true |> insert 1 |>  uncons
    x |> should equal 1

[<Test>]
let ``uncons 2 elements``() =
    let x, _ = empty true |> insert 1 |> insert 2 |> uncons 
    x |> should equal 2

[<Test>]
let ``uncons 3 elements``() =
    let x, _ = empty false |> insert 1 |> insert 2 |> insert 3 |> uncons 
    x |> should equal 1

[<Test>]
let ``tryUncons 1 element``() =
    let x = empty false |> insert 1 |> tryUncons
    fst(x.Value)  |> should equal 1

[<Test>]
let ``tryUncons 2 elements``() =
    let x = empty false |> insert 1 |> insert 2 |> tryUncons
    fst(x.Value) |> should equal 1

[<Test>]
let ``tryUncons 3 elements``() =
    let x = empty true |> insert 1 |> insert 2 |> insert 3 |> tryUncons 
    fst(x.Value) |> should equal 3

[<Test>]
let ``tryUncons empty``() =
    empty false |> tryUncons |> should equal None
    
[<Test>]
let ``head should return``() =
    let x = empty true |> insert 1 |> insert 2 |> head 
    x |> should equal 2

[<Test>]
let ``tryGetHead should return``() =
    let x = empty false |> insert 1 |> insert 2 |> tryGetHead 
    x.Value |> should equal 1

[<Test>]
let ``tryGetHead on empty should return None``() =
    empty true |> tryGetHead |> should equal None

[<Test>]
let ``tryGetTail on empty should return None``() =
    empty false |> tryGetTail |> should equal None

[<Test>]
let ``tryGetTail on len 1 should return Some empty``() =
    let x = (empty true |> insert 1 |> tryGetTail).Value
    x |> isEmpty |> should equal true

[<Test>]
let ``tail on len 2 should return``() =
    empty true |> insert 1 |>  insert 2 |> tail |> head |> should equal 1

[<Test>]
let ``tryGetTail on len 2 should return``() =
    let a = empty false |> insert 1 |>  insert 2 |> tryGetTail 
    ((head a.Value) = 2) |> should equal true

[<Test>]
let ``length of empty is 0``() =
    empty false |> length |> should equal 0

[<Test>]
let ``length of 1 - 10 good``() =

    let len1 = empty true |> insert "a"
    let len2 = empty true |> insert "a" |> insert "b"
    let len3 = empty true |> insert "a" |> insert "b" |> insert "c"
    let len4 = empty true |> insert "a" |> insert "b" |> insert "c" |> insert "d"
    let len5 = empty true |> insert "a" |> insert "b" |> insert "c" |> insert "d" |> insert "e"
    let len6 = empty true |> insert "a" |> insert "b" |> insert "c" |> insert "d" |> insert "e" |> insert "f"
    let len7 = empty true |> insert "a" |> insert "b" |> insert "c" |> insert "d" |> insert "e" |> insert "f" |> insert "g"
    let len8 = empty true |> insert "a" |> insert "b" |> insert "c" |> insert "d" |> insert "e" |> insert "f" |> insert "g" |> insert "h"
    let len9 = empty true |> insert "a" |> insert "b" |> insert "c" |> insert "d" |> insert "e" |> insert "f" |> insert "g" |> insert "h" |> insert "i"
    let lena = empty true |> insert "a" |> insert "b" |> insert "c" |> insert "d" |> insert "e" |> insert "f" |> insert "g" |> insert "h" |> insert "i" |> insert "j"

    (((length len1) = 1) && ((length len2) = 2) && ((length len3) = 3) && ((length len4) = 4) 
    && ((length len5) = 5) && ((length len6) = 6) && ((length len7) = 7) && ((length len8) = 8) 
    && ((length len9) = 9) && ((length lena) = 10)) |> should equal true

[<Test>]
let ``ofSeq minimalist``() =
    let x = ofSeq false ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

    let a, t1 = uncons x
    let b, t2 = uncons t1
    let c, t3 = uncons t2
    let d, t4 = uncons t3
    let e, t5 = uncons t4
    let f, t6 = uncons t5
    let g, t7 = uncons t6
    let h, t8 = uncons t7
    let i, t9 = uncons t8
    let j, t10 = uncons t9
    ((a = "a") && (b = "b") && (c = "c") && (d = "d") && (e = "e") && (f = "f") && (g = "g") && (h = "h") 
    && (i = "i") && (j = "j") && (t10 |> isEmpty)) |> should equal true

[<Test>]
let ``ofSeq minimalist odd number``() =
    let x = ofSeq false ["a";"b";"c";"d";"e";"f";"g";"h";"i"]

    let a, t1 = uncons x
    let b, t2 = uncons t1
    let c, t3 = uncons t2
    let d, t4 = uncons t3
    let e, t5 = uncons t4
    let f, t6 = uncons t5
    let g, t7 = uncons t6
    let h, t8 = uncons t7
    let i, t9 = uncons t8
    ((a = "a") && (b = "b") && (c = "c") && (d = "d") && (e = "e") && (f = "f") && (g = "g") && (h = "h") 
    && (i = "i") && (t9 |> isEmpty)) |> should equal true

[<Test>]
let ``ofSeq maximalist``() =
    let x = ofSeq true ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

    let j, t1 = uncons x
    let i, t2 = uncons t1
    let h, t3 = uncons t2
    let g, t4 = uncons t3
    let f, t5 = uncons t4
    let e, t6 = uncons t5
    let d, t7 = uncons t6
    ((d = "d") && (e = "e") && (f = "f") && (g = "g") && (h = "h") && (i = "i") && (j = "j")) |> should equal true

[<Test>]
let ``IRandomAccessList insert works``() =
    let lena = empty true |> insert "a" |> insert "b" |> insert "c" |> insert "d" |> insert "e" |> insert "f" |> insert "g" |> insert "h" |> insert "i" |> insert "j"
    ((lena :> IHeap<string>).Insert "zz") :?> LeftistHeap<string> |> head |> should equal "zz"

[<Test>]
let ``cons pattern discriminator``() =
    let h = ofSeq true ["f";"e";"d";"c";"b";"a"]
    let h1, t1 = uncons h 

    let h2, t2 = 
        match t1 with
        | Cons(h, t) -> h, t
        | _ ->  "x", t1

    ((h2 = "e") && ((length t2) = 4)) |> should equal true

[<Test>]
let ``cons pattern discriminator 2``() =
    let h = ofSeq true ["f";"e";"d";"c";"b";"a"]

    let t2 = 
        match h with
        | Cons("f", Cons(_, t)) -> t
        | _ ->  h

    let h1, t3 = uncons t2 

    ((h1 = "d") && ((length t2) = 4)) |> should equal true

[<Test>]
let ``structure pattern match and merge``() =
    let h = ofSeq true ["f";"e";"d";"c";"b";"a"]

    let x, h1, h2 = 
        match h with
        | T(_, _, _, x', h1', h2') -> x', h1', h2'
        | _ ->  "zz", h, h

    let h3 = merge h1 h2 

    let x2, t3 = uncons h3 

    ((x = "f") && (x2 = "e") && ((length t3) = 4)) |> should equal true

[<Test>]
let ``tryMerge max and mis should be None``() =
    let h1 = ofSeq true ["f";"e";"d";"c";"b";"a"]
    let h2 = ofSeq false ["t";"u";"v";"w";"x";"y";"z"]

    tryMerge h1 h2 |> should equal None