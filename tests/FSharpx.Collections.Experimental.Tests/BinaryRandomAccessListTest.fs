module FSharpx.Collections.Experimental.Tests.BinaryRandomAccessListTest

open System
open FSharpx.Collections.Experimental
open FSharpx.Collections.Experimental.BinaryRandomAccessList
open NUnit.Framework
open FsUnit


//only going up to len5 is probably sufficient to test all edge cases
//but better too many unit tests than too few

let len1 = empty() |> cons "a"
let len2 = empty() |> cons "a" |> cons "b"
let len3 = empty() |> cons "a" |> cons "b" |> cons "c"
let len4 = empty() |> cons "a" |> cons "b" |> cons "c" |> cons "d"
let len5 = empty() |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e"
let len6 = empty() |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f"
let len7 = empty() |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g"
let len8 = empty() |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h"
let len9 = empty() |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i"
let lena = empty() |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i" |> cons "j"

[<Test>]
let ``empty list should be empty``() =
    empty() |> isEmpty |> should equal true

[<Test>]
let ``cons works``() =
    empty()|> cons 1 |> cons 2 |> isEmpty |> should equal false

[<Test>]
let ``uncons 1 element``() =
    let x, _ = empty() |> cons 1 |>  uncons
    (x = 1) |> should equal true

[<Test>]
let ``uncons 2 elements``() =
    let x, _ = empty() |> cons 1 |> cons 2 |> uncons 
    (x = 2) |> should equal true

[<Test>]
let ``uncons 3 elements``() =
    let x, _ = empty() |> cons 1 |> cons 2 |> cons 3 |> uncons 
    (x = 3) |> should equal true

[<Test>]
let ``tryUncons 1 element``() =
    let x = empty() |> cons 1 |> tryUncons
    (fst(x.Value) = 1) |> should equal true

[<Test>]
let ``tryUncons 2 elements``() =
    let x = empty() |> cons 1 |> cons 2 |> tryUncons
    (fst(x.Value) = 2) |> should equal true

[<Test>]
let ``tryUncons 3 elements``() =
    let x = empty() |> cons 1 |> cons 2 |> cons 3 |> tryUncons 
    (fst(x.Value) = 3) |> should equal true

[<Test>]
let ``tryUncons empty``() =
    empty() |> tryUncons |> should equal None
    
[<Test>]
let ``head should return``() =
    let x = empty() |> cons 1 |> cons 2 |> head 
    x |> should equal 2

[<Test>]
let ``tryGetHead should return``() =
    let x = empty() |> cons 1 |> cons 2 |> tryGetHead 
    x.Value |> should equal 2

[<Test>]
let ``tryGetHead on empty should return None``() =
    empty() |> tryGetHead |> should equal None

[<Test>]
let ``tryGetTail on empty should return None``() =
    empty() |> tryGetTail |> should equal None

[<Test>]
let ``tryGetTail on len 1 should return Some empty``() =
    let x = (empty() |> cons 1 |> tryGetTail).Value
    x |> isEmpty |> should equal true

[<Test>]
let ``tail on len 2 should return``() =
    empty() |> cons 1 |>  cons 2 |> tail |> head |> should equal 1

[<Test>]
let ``tryGetTail on len 2 should return``() =
    let a = empty() |> cons 1 |>  cons 2 |> tryGetTail 
    ((head a.Value) = 1) |> should equal true

[<Test>]
let ``lookup length 1``() =
    len1 |> lookup 0 |> should equal "a"

[<Test>]
let ``rev empty``() =
    isEmpty (empty() |> rev) |> should equal true
    
[<Test>]
let ``rev elements length 5``() =
    let a = ofSeq ["a";"b";"c";"d";"e"]

    let b = rev a

    let c = List.ofSeq b

    c.Head |> should equal "e"

[<Test>]
let ``lookup length 2``() =
    (((len2 |> lookup 0) = "b") && ((len2 |> lookup 1) = "a")) |> should equal true

[<Test>]
let ``lookup length 3``() =
    (((len3 |> lookup 0) = "c") && ((len3 |> lookup 1) = "b") && ((len3 |> lookup 2) = "a")) |> should equal true

[<Test>]
let ``lookup length 4``() =
    (((len4 |> lookup 0) = "d") && ((len4 |> lookup 1) = "c") && ((len4 |> lookup 2) = "b") && ((len4 |> lookup 3) = "a")) 
    |> should equal true

[<Test>]
let ``lookup length 5``() =
    (((len5 |> lookup 0) = "e") && ((len5 |> lookup 1) = "d") && ((len5 |> lookup 2) = "c") && ((len5 |> lookup 3) = "b") 
    && ((len5 |> lookup 4) = "a")) |> should equal true

[<Test>]
let ``lookup length 6``() =
    (((len6 |> lookup 0) = "f") && ((len6 |> lookup 1) = "e") && ((len6 |> lookup 2) = "d") && ((len6 |> lookup 3) = "c") 
    && ((len6 |> lookup 4) = "b") && ((len6 |> lookup 5) = "a")) |> should equal true

[<Test>]
let ``lookup length 7``() =
    (((len7 |> lookup 0) = "g") && ((len7 |> lookup 1) = "f") && ((len7 |> lookup 2) = "e") && ((len7 |> lookup 3) = "d") 
    && ((len7 |> lookup 4) = "c") && ((len7 |> lookup 5) = "b") && ((len7 |> lookup 6) = "a")) |> should equal true

[<Test>]
let ``lookup length 8``() =
    (((len8 |> lookup 0) = "h") && ((len8 |> lookup 1) = "g") && ((len8 |> lookup 2) = "f") && ((len8 |> lookup 3) = "e") 
    && ((len8 |> lookup 4) = "d") && ((len8 |> lookup 5) = "c") && ((len8 |> lookup 6) = "b") && ((len8 |> lookup 7) = "a")) 
    |> should equal true

[<Test>]
let ``lookup length 9``() =
    (((len9 |> lookup 0) = "i") && ((len9 |> lookup 1) = "h") && ((len9 |> lookup 2) = "g") && ((len9 |> lookup 3) = "f") 
    && ((len9 |> lookup 4) = "e") && ((len9 |> lookup 5) = "d") && ((len9 |> lookup 6) = "c") && ((len9 |> lookup 7) = "b")
    && ((len9 |> lookup 8) = "a")) |> should equal true

[<Test>]
let ``lookup length 10``() =
    (((lena |> lookup 0) = "j") && ((lena |> lookup 1) = "i") && ((lena |> lookup 2) = "h") && ((lena |> lookup 3) = "g") 
    && ((lena |> lookup 4) = "f") && ((lena |> lookup 5) = "e") && ((lena |> lookup 6) = "d") && ((lena |> lookup 7) = "c")
    && ((lena |> lookup 8) = "b") && ((lena |> lookup 9) = "a")) |> should equal true

[<Test>]
let ``tryLookup length 1``() =
    let a = len1 |> tryLookup 0 
    (a.Value = "a") |> should equal true

[<Test>]
let ``tryLookup length 2``() =
    let b = len2 |> tryLookup 0
    let a = len2 |> tryLookup 1
    ((b.Value = "b") && (a.Value = "a")) |> should equal true

[<Test>]
let ``tryLookup length 3``() =
    let c = len3 |> tryLookup 0
    let b = len3 |> tryLookup 1
    let a = len3 |> tryLookup 2
    ((c.Value = "c") && (b.Value = "b") && (a.Value = "a")) |> should equal true

[<Test>]
let ``tryLookup length 4``() =
    let d = len4 |> tryLookup 0
    let c = len4 |> tryLookup 1
    let b = len4 |> tryLookup 2
    let a = len4 |> tryLookup 3
    ((d.Value = "d") && (c.Value = "c") && (b.Value = "b") && (a.Value = "a")) |> should equal true 

[<Test>]
let ``tryLookup length 5``() =
    let e = len5 |> tryLookup 0
    let d = len5 |> tryLookup 1
    let c = len5 |> tryLookup 2
    let b = len5 |> tryLookup 3
    let a = len5 |> tryLookup 4
    ((e.Value = "e") && (d.Value = "d") && (c.Value = "c") && (b.Value = "b") && (a.Value = "a")) |> should equal true

[<Test>]
let ``tryLookup length 6``() =
    let f = len6 |> tryLookup 0
    let e = len6 |> tryLookup 1
    let d = len6 |> tryLookup 2
    let c = len6 |> tryLookup 3
    let b = len6 |> tryLookup 4
    let a = len6 |> tryLookup 5
    ((f.Value = "f") && (e.Value = "e") && (d.Value = "d") && (c.Value = "c") && (b.Value = "b") && (a.Value = "a")) 
    |> should equal true

[<Test>]
let ``tryLookup length 7``() =
    let g = len7 |> tryLookup 0
    let f = len7 |> tryLookup 1
    let e = len7 |> tryLookup 2
    let d = len7 |> tryLookup 3
    let c = len7 |> tryLookup 4
    let b = len7 |> tryLookup 5
    let a = len7 |> tryLookup 6
    ((g.Value = "g") && (f.Value = "f") && (e.Value = "e") && (d.Value = "d") && (c.Value = "c") && (b.Value = "b") 
    && (a.Value = "a")) |> should equal true

[<Test>]
let ``tryLookup length 8``() =
    let h = len8 |> tryLookup 0
    let g = len8 |> tryLookup 1
    let f = len8 |> tryLookup 2
    let e = len8 |> tryLookup 3
    let d = len8 |> tryLookup 4
    let c = len8 |> tryLookup 5
    let b = len8 |> tryLookup 6
    let a = len8 |> tryLookup 7
    ((h.Value = "h") && (g.Value = "g") && (f.Value = "f") && (e.Value = "e") && (d.Value = "d") && (c.Value = "c")  
    && (b.Value = "b")&& (a.Value = "a")) |> should equal true

[<Test>]
let ``tryLookup length 9``() =
    let i = len9 |> tryLookup 0
    let h = len9 |> tryLookup 1
    let g = len9 |> tryLookup 2
    let f = len9 |> tryLookup 3
    let e = len9 |> tryLookup 4
    let d = len9 |> tryLookup 5
    let c = len9 |> tryLookup 6
    let b = len9 |> tryLookup 7
    let a = len9 |> tryLookup 8
    ((i.Value = "i") && (h.Value = "h") && (g.Value = "g") && (f.Value = "f") && (e.Value = "e") && (d.Value = "d") 
    && (c.Value = "c") && (b.Value = "b")&& (a.Value = "a")) |> should equal true

[<Test>]
let ``tryLookup length 10``() =
    let j = lena |> tryLookup 0
    let i = lena |> tryLookup 1
    let h = lena |> tryLookup 2
    let g = lena |> tryLookup 3
    let f = lena |> tryLookup 4
    let e = lena |> tryLookup 5
    let d = lena |> tryLookup 6
    let c = lena |> tryLookup 7
    let b = lena |> tryLookup 8
    let a = lena |> tryLookup 9
    ((j.Value = "j") && (i.Value = "i") && (h.Value = "h") && (g.Value = "g") && (f.Value = "f") && (e.Value = "e") 
    && (d.Value = "d") && (c.Value = "c") && (b.Value = "b")&& (a.Value = "a")) |> should equal true

[<Test>]
let ``tryLookup not found``() =
    lena |> tryLookup 10 |> should equal None

[<Test>]
let ``update length 1``() =
    len1 |> update 0 "aa"|> lookup 0 |> should equal "aa"

[<Test>]
let ``update length 2``() =
    (((len2 |> update 0 "bb"|> lookup 0) = "bb") && ((len2 |> update 1 "aa"|> lookup 1) = "aa")) |> should equal true

[<Test>]
let ``update length 3``() =
    (((len3 |> update 0 "cc"|> lookup 0) = "cc") && ((len3 |> update 1 "bb"|> lookup 1) = "bb") 
    && ((len3 |> update 2 "aa"|> lookup 2) = "aa")) |> should equal true

[<Test>]
let ``update length 4``() =
    (((len4 |> update 0 "dd"|> lookup 0) = "dd") && ((len4 |> update 1 "cc"|> lookup 1) = "cc") 
    && ((len4 |> update 2 "bb"|> lookup 2) = "bb") && ((len4 |> update 3 "aa"|> lookup 3) = "aa")) 
    |> should equal true

[<Test>]
let ``update length 5``() =
    (((len5 |> update 0 "ee"|> lookup 0) = "ee") && ((len5 |> update 1 "dd"|> lookup 1) = "dd") 
    && ((len5 |> update 2 "cc"|> lookup 2) = "cc") && ((len5 |> update 3 "bb"|> lookup 3) = "bb") 
    && ((len5 |> update 4 "aa"|> lookup 4) = "aa")) |> should equal true

[<Test>]
let ``update length 6``() =
    (((len6 |> update 0 "ff"|> lookup 0) = "ff") && ((len6 |> update 1 "ee"|> lookup 1) = "ee") 
    && ((len6 |> update 2 "dd"|> lookup 2) = "dd") && ((len6 |> update 3 "cc"|> lookup 3) = "cc") 
    && ((len6 |> update 4 "bb"|> lookup 4) = "bb") && ((len6 |> update 5 "aa"|> lookup 5) = "aa")) |> should equal true

[<Test>]
let ``update length 7``() =
    (((len7 |> update 0 "gg"|> lookup 0) = "gg") && ((len7 |> update 1 "ff"|> lookup 1) = "ff") 
    && ((len7 |> update 2 "ee"|> lookup 2) = "ee") && ((len7 |> update 3 "dd"|> lookup 3) = "dd") 
    && ((len7 |> update 4 "cc"|> lookup 4) = "cc") && ((len7 |> update 5 "bb"|> lookup 5) = "bb") 
    && ((len7 |> update 6 "aa"|> lookup 6) = "aa")) |> should equal true

[<Test>]
let ``update length 8``() =
    (((len8 |> update 0 "hh"|> lookup 0) = "hh") && ((len8 |> update 1 "gg"|> lookup 1) = "gg") 
    && ((len8 |> update 2 "ff"|> lookup 2) = "ff") && ((len8 |> update 3 "ee"|> lookup 3) = "ee") 
    && ((len8 |> update 4 "dd"|> lookup 4) = "dd") && ((len8 |> update 5 "cc"|> lookup 5) = "cc") 
    && ((len8 |> update 6 "bb"|> lookup 6) = "bb") && ((len8 |> update 7 "aa"|> lookup 7) = "aa")) 
    |> should equal true

[<Test>]
let ``update length 9``() =
    (((len9 |> update 0 "ii"|> lookup 0) = "ii") && ((len9 |> update 1 "hh"|> lookup 1) = "hh") 
    && ((len9 |> update 2 "gg"|> lookup 2) = "gg") && ((len9 |> update 3 "ff"|> lookup 3) = "ff") 
    && ((len9 |> update 4 "ee"|> lookup 4) = "ee") && ((len9 |> update 5 "dd"|> lookup 5) = "dd") 
    && ((len9 |> update 6 "cc"|> lookup 6) = "cc") && ((len9 |> update 7 "bb"|> lookup 7) = "bb")
    && ((len9 |> update 8 "aa"|> lookup 8) = "aa")) |> should equal true

[<Test>]
let ``update length 10``() =
    (((lena |> update 0 "jj"|> lookup 0) = "jj") && ((lena |> update 1 "ii"|> lookup 1) = "ii") 
    && ((lena |> update 2 "hh"|> lookup 2) = "hh") && ((lena |> update 3 "gg"|> lookup 3) = "gg") 
    && ((lena |> update 4 "ff"|> lookup 4) = "ff") && ((lena |> update 5 "ee"|> lookup 5) = "ee") 
    && ((lena |> update 6 "dd"|> lookup 6) = "dd") && ((lena |> update 7 "cc"|> lookup 7) = "cc")
    && ((lena |> update 8 "bb"|> lookup 8) = "bb") && ((lena |> update 9 "aa"|> lookup 9) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 1``() =
    let a = len1 |> tryUpdate 0 "aa"
    ((a.Value |> lookup 0) = "aa") |> should equal true

[<Test>]
let ``tryUpdate length 2``() =
    let b = len2 |> tryUpdate 0 "bb"
    let a = len2 |> tryUpdate 1 "aa"
    (((b.Value |> lookup 0) = "bb") && ((a.Value |> lookup 1) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 3``() =
    let c = len3 |> tryUpdate 0 "cc"
    let b = len3 |> tryUpdate 1 "bb"
    let a = len3 |> tryUpdate 2 "aa"
    (((c.Value |> lookup 0) = "cc") && ((b.Value |> lookup 1) = "bb") && ((a.Value |> lookup 2) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 4``() =
    let d = len4 |> tryUpdate 0 "dd"
    let c = len4 |> tryUpdate 1 "cc"
    let b = len4 |> tryUpdate 2 "bb"
    let a = len4 |> tryUpdate 3 "aa"
    (((d.Value |> lookup 0) = "dd") && ((c.Value |> lookup 1) = "cc") && ((b.Value |> lookup 2) = "bb") 
    && ((a.Value |> lookup 3) = "aa")) |> should equal true 

[<Test>]
let ``tryUpdate length 5``() =
    let e = len5 |> tryUpdate 0 "ee"
    let d = len5 |> tryUpdate 1 "dd"
    let c = len5 |> tryUpdate 2 "cc"
    let b = len5 |> tryUpdate 3 "bb"
    let a = len5 |> tryUpdate 4 "aa"
    (((e.Value |> lookup 0) = "ee") && ((d.Value |> lookup 1) = "dd") && ((c.Value |> lookup 2) = "cc") 
    && ((b.Value |> lookup 3) = "bb") && ((a.Value |> lookup 4) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 6``() =
    let f = len6 |> tryUpdate 0 "ff"
    let e = len6 |> tryUpdate 1 "ee"
    let d = len6 |> tryUpdate 2 "dd"
    let c = len6 |> tryUpdate 3 "cc"
    let b = len6 |> tryUpdate 4 "bb"
    let a = len6 |> tryUpdate 5 "aa"
    (((f.Value |> lookup 0) = "ff") && ((e.Value |> lookup 1) = "ee") && ((d.Value |> lookup 2) = "dd") 
    && ((c.Value |> lookup 3) = "cc") && ((b.Value |> lookup 4) = "bb") && ((a.Value |> lookup 5) = "aa")) 
    |> should equal true

[<Test>]
let ``tryUpdate length 7``() =
    let g = len7 |> tryUpdate 0 "gg"
    let f = len7 |> tryUpdate 1 "ff"
    let e = len7 |> tryUpdate 2 "ee"
    let d = len7 |> tryUpdate 3 "dd"
    let c = len7 |> tryUpdate 4 "cc"
    let b = len7 |> tryUpdate 5 "bb"
    let a = len7 |> tryUpdate 6 "aa"
    (((g.Value |> lookup 0) = "gg") && ((f.Value |> lookup 1) = "ff") && ((e.Value |> lookup 2) = "ee") 
    && ((d.Value |> lookup 3) = "dd") && ((c.Value |> lookup 4) = "cc") && ((b.Value |> lookup 5) = "bb") 
    && ((a.Value |> lookup 6) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 8``() =
    let h = len8 |> tryUpdate 0 "hh"
    let g = len8 |> tryUpdate 1 "gg"
    let f = len8 |> tryUpdate 2 "ff"
    let e = len8 |> tryUpdate 3 "ee"
    let d = len8 |> tryUpdate 4 "dd"
    let c = len8 |> tryUpdate 5 "cc"
    let b = len8 |> tryUpdate 6 "bb"
    let a = len8 |> tryUpdate 7 "aa"
    (((h.Value |> lookup 0) = "hh") && ((g.Value |> lookup 1) = "gg") && ((f.Value |> lookup 2) = "ff") 
    && ((e.Value |> lookup 3) = "ee") && ((d.Value |> lookup 4) = "dd") && ((c.Value |> lookup 5) = "cc")  
    && ((b.Value |> lookup 6) = "bb")&& ((a.Value |> lookup 7) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 9``() =
    let i = len9 |> tryUpdate 0 "ii"
    let h = len9 |> tryUpdate 1 "hh"
    let g = len9 |> tryUpdate 2 "gg"
    let f = len9 |> tryUpdate 3 "ff"
    let e = len9 |> tryUpdate 4 "ee"
    let d = len9 |> tryUpdate 5 "dd"
    let c = len9 |> tryUpdate 6 "cc"
    let b = len9 |> tryUpdate 7 "bb"
    let a = len9 |> tryUpdate 8 "aa"
    (((i.Value |> lookup 0) = "ii") && ((h.Value |> lookup 1) = "hh") && ((g.Value |> lookup 2) = "gg") 
    && ((f.Value |> lookup 3) = "ff") && ((e.Value |> lookup 4) = "ee") && ((d.Value |> lookup 5) = "dd") 
    && ((c.Value |> lookup 6) = "cc") && ((b.Value |> lookup 7) = "bb")&& ((a.Value |> lookup 8) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 10``() =
    let j = lena |> tryUpdate 0 "jj"
    let i = lena |> tryUpdate 1 "ii"
    let h = lena |> tryUpdate 2 "hh"
    let g = lena |> tryUpdate 3 "gg"
    let f = lena |> tryUpdate 4 "ff"
    let e = lena |> tryUpdate 5 "ee"
    let d = lena |> tryUpdate 6 "dd"
    let c = lena |> tryUpdate 7 "cc"
    let b = lena |> tryUpdate 8 "bb"
    let a = lena |> tryUpdate 9 "aa"
    (((j.Value |> lookup 0) = "jj") && ((i.Value |> lookup 1) = "ii") && ((h.Value |> lookup 2) = "hh") 
    && ((g.Value |> lookup 3) = "gg") && ((f.Value |> lookup 4) = "ff") && ((e.Value |> lookup 5) = "ee") 
    && ((d.Value |> lookup 6) = "dd") && ((c.Value |> lookup 7) = "cc") && ((b.Value |> lookup 8) = "bb")
    && ((a.Value |> lookup 9) = "aa")) |> should equal true

[<Test>]
let ``length of empty is 0``() =
    empty() |> length |> should equal 0

[<Test>]
let ``length of 1 - 10 good``() =

    (((length len1) = 1) && ((length len2) = 2) && ((length len3) = 3) && ((length len4) = 4) 
    && ((length len5) = 5) && ((length len6) = 6) && ((length len7) = 7) && ((length len8) = 8) 
    && ((length len9) = 9) && ((length lena) = 10)) |> should equal true

[<Test>]
let ``ofSeq``() =
    let x = ofSeq ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

    (((x |> lookup 0) = "a") && ((x |> lookup 1) = "b") && ((x |> lookup 2) = "c") && ((x |> lookup 3) = "d") 
    && ((x |> lookup 4) = "e") && ((x |> lookup 5) = "f") && ((x |> lookup 6) = "g") && ((x |> lookup 7) = "h")
    && ((x |> lookup 8) = "i") && ((x |> lookup 9) = "j")) |> should equal true

[<Test>]
let ``IRandomAccessList cons works``() =
    ((lena :> IRandomAccessList<string>).Cons "zz").Head |> should equal "zz"