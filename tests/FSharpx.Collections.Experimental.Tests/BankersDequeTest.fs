module FSharpx.Collections.Experimental.Tests.BankersDequeTest

open System
open FSharpx.Collections.Experimental
open FSharpx.Collections.Experimental.BankersDeque
open NUnit.Framework
open FsUnit


//quite a lot going on and difficult to reason about edge cases
//testing up to length of 6 is the likely minimum to satisfy any arbitrary test case (less for some cases)
//6 makes front and back lists 3 long when C = 2

let len1 = singleton "a"
let len2 = singleton "a" |> cons "b"
let len3 = singleton "a" |> cons "b" |> cons "c"
let len4 = singleton "a" |> cons "b" |> cons "c" |> cons "d"
let len5 = singleton "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e"
let len6 = singleton "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f"
let len7 = singleton "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g"
let len8 = singleton "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h"
let len9 = singleton "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i"
let lena = singleton "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i" |> cons "j"

let len1snoc = singleton "a"
let len2snoc = singleton "b" |> snoc "a"
let len3snoc = singleton "c" |> snoc "b" |> snoc "a"
let len4snoc = singleton "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len5snoc = singleton "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len6snoc = singleton "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len7snoc = singleton "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len8snoc = singleton "h" |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len9snoc = singleton "i" |> snoc "h" |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let lenasnoc = singleton "j" |> snoc "i" |> snoc "h" |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"

let len1C3 = empty 3 |> cons "a"
let len2C3 = empty 3 |> cons "a" |> cons "b"
let len3C3 = empty 3 |> cons "a" |> cons "b" |> cons "c"
let len4C3 = empty 3 |> cons "a" |> cons "b" |> cons "c" |> cons "d"
let len5C3 = empty 3 |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e"
let len6C3 = empty 3 |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f"
let len7C3 = empty 3 |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g"
let len8C3 = empty 3 |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h"
let len9C3 = empty 3 |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i"
let lenaC3 = empty 3 |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i" |> cons "j"

let len1C3snoc = empty 3 |> snoc "a"
let len2C3snoc = empty 3 |> snoc "b" |> snoc "a"
let len3C3snoc = empty 3 |> snoc "c" |> snoc "b" |> snoc "a"
let len4C3snoc = empty 3 |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len5C3snoc = empty 3 |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len6C3snoc = empty 3 |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len7C3snoc = empty 3 |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len8C3snoc = empty 3 |> snoc "h" |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len9C3snoc = empty 3 |> snoc "i" |> snoc "h" |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let lenaC3snoc = empty 3 |> snoc "j" |> snoc "i" |> snoc "h" |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"

[<Test>]
let ``empty dqueue should be empty``() =

    isEmpty (empty 2) |> should equal true

[<Test>]
let ``cons works``() =
    ((len2 |> isEmpty) && (len2C3 |> isEmpty)) |> should equal false

[<Test>]
let ``snoc works``() =
    ((len2snoc |> isEmpty) && (len2C3snoc |> isEmpty)) |> should equal false

[<Test>]
let ``singleton head works``() =
    (((head len1) = "a") && ((len1C3 |> isEmpty)) = false) |> should equal true

[<Test>]
let ``singleton last works``() =
    len1 |> last |> should equal "a"

[<Test>]
let ``tail of singleton empty``() =
    len1 |> tail |> isEmpty |> should equal true

[<Test>]
let ``tail of tail of 2 empty``() =
     ( len2 |> tail |> tail |> isEmpty) |> should equal true

[<Test>]
let ``init of singleton empty``() =
    ((init len1)  |> isEmpty) |> should equal true

[<Test>]
let ``head, tail, and length work test 1``() =
    let t1 = tail len2
    let t1C = tail len2C3
    let t1s = tail len2snoc
    let t1Cs = tail len2C3snoc
    (((length t1) = 1) && ((length t1C) = 1) && ((length t1s) = 1) && ((length t1Cs) = 1) 
    && ((head t1) = "a") && ((head t1C) = "a") && ((head t1s) = "a") && ((head t1Cs) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 2``() =
    let t1 = tail len3
    let t1C = tail len3C3
    let t1s = tail len3snoc
    let t1Cs = tail len3C3snoc

    let t1_1 = tail t1
    let t1C_1 = tail t1C
    let t1_1s = tail t1s
    let t1C_1s = tail t1Cs

    (((length t1) = 2) && ((length t1C) = 2) && ((length t1s) = 2) && ((length t1Cs) = 2)
    && ((head t1) = "b") && ((head t1C) = "b") && ((head t1s) = "b") && ((head t1Cs) = "b")
    && ((length t1_1) = 1) && ((length t1C_1) = 1) && ((length t1_1s) = 1) && ((length t1C_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1C_1) = "a") && ((head t1_1s) = "a") && ((head t1C_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 3``() =
    let t1 = tail len4
    let t1C = tail len4C3
    let t1s = tail len4snoc
    let t1Cs = tail len4C3snoc

    let t1_2 = tail t1
    let t1C_2 = tail t1C
    let t1_2s = tail t1s
    let t1C_2s = tail t1Cs

    let t1_1 = tail t1_2
    let t1C_1 = tail t1C_2
    let t1_1s = tail t1_2s
    let t1C_1s = tail t1C_2s

    (((length t1) = 3) && ((length t1C) = 3) && ((length t1s) = 3) && ((length t1Cs) = 3)
    && ((head t1) = "c") && ((head t1C) = "c") && ((head t1s) = "c") && ((head t1Cs) = "c")
    && ((length t1_2) = 2) && ((length t1C_2) = 2) && ((length t1_2s) = 2) && ((length t1C_2s) = 2)
    && ((head t1_2) = "b") && ((head t1C_2) = "b") && ((head t1_2s) = "b") && ((head t1C_2s) = "b")
    && ((length t1_1) = 1) && ((length t1C_1) = 1) && ((length t1_1s) = 1) && ((length t1C_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1C_1) = "a") && ((head t1_1s) = "a") && ((head t1C_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 4``() =
    let t1 = tail len5
    let t1C = tail len5C3
    let t1s = tail len5snoc
    let t1Cs = tail len5C3snoc

    let t1_3 = tail t1
    let t1C_3 = tail t1C
    let t1_3s = tail t1s
    let t1C_3s = tail t1Cs

    let t1_2 = tail t1_3
    let t1C_2 = tail t1C_3
    let t1_2s = tail t1_3s
    let t1C_2s = tail t1C_3s

    let t1_1 = tail t1_2
    let t1C_1 = tail t1C_2
    let t1_1s = tail t1_2s
    let t1C_1s = tail t1C_2s

    (((length t1) = 4) && ((length t1C) = 4) && ((length t1s) = 4) && ((length t1Cs) = 4)
    && ((head t1) = "d") && ((head t1C) = "d") && ((head t1s) = "d") && ((head t1Cs) = "d")
    && ((length t1_3) = 3) && ((length t1C_3) = 3) && ((length t1_3s) = 3) && ((length t1C_3s) = 3)
    && ((head t1_3) = "c") && ((head t1C_3) = "c") && ((head t1_3s) = "c") && ((head t1C_3s) = "c")
    && ((length t1_2) = 2) && ((length t1C_2) = 2) && ((length t1_2s) = 2) && ((length t1C_2s) = 2) 
    && ((head t1_2) = "b") && ((head t1C_2) = "b") && ((head t1_2s) = "b") && ((head t1C_2s) = "b")
    && ((length t1_1) = 1) && ((length t1C_1) = 1) && ((length t1_1s) = 1) && ((length t1C_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1C_1) = "a") && ((head t1_1s) = "a") && ((head t1C_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 5``() =
    let t1 = tail len6
    let t1C = tail len6C3
    let t1s = tail len6snoc
    let t1Cs = tail len6C3snoc

    let t1_4 = tail t1
    let t1C_4 = tail t1C
    let t1_4s = tail t1s
    let t1C_4s = tail t1Cs

    let t1_3 = tail t1_4
    let t1C_3 = tail t1C_4
    let t1_3s = tail t1_4s
    let t1C_3s = tail t1C_4s

    let t1_2 = tail t1_3
    let t1C_2 = tail t1C_3
    let t1_2s = tail t1_3s
    let t1C_2s = tail t1C_3s

    let t1_1 = tail t1_2
    let t1C_1 = tail t1C_2
    let t1_1s = tail t1_2s
    let t1C_1s = tail t1C_2s

    (((length t1) = 5) && ((length t1C) = 5) && ((length t1s) = 5) && ((length t1Cs) = 5)
    && ((head t1) = "e") && ((head t1C) = "e") && ((head t1s) = "e") && ((head t1Cs) = "e")
    && ((length t1_4) = 4) && ((length t1C_4) = 4) && ((length t1_4s) = 4) && ((length t1C_4s) = 4)
    && ((head t1_4) = "d") && ((head t1C_4) = "d") && ((head t1_4s) = "d") && ((head t1C_4s) = "d")
    && ((length t1_3) = 3) && ((length t1C_3) = 3) && ((length t1_3s) = 3) && ((length t1C_3s) = 3)
    && ((head t1_3) = "c") && ((head t1C_3) = "c") && ((head t1_3s) = "c") && ((head t1C_3s) = "c")
    && ((length t1_2) = 2) && ((length t1C_2) = 2) && ((length t1_2s) = 2) && ((length t1C_2s) = 2) 
    && ((head t1_2) = "b") && ((head t1C_2) = "b") && ((head t1_2s) = "b") && ((head t1C_2s) = "b")
    && ((length t1_1) = 1) && ((length t1C_1) = 1) && ((length t1_1s) = 1) && ((length t1C_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1C_1) = "a") && ((head t1_1s) = "a") && ((head t1C_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 6``() =
    let t1 = tail len7
    let t1C = tail len7C3
    let t1s = tail len7snoc
    let t1Cs = tail len7C3snoc

    let t1_5 = tail t1
    let t1C_5 = tail t1C
    let t1_5s = tail t1s
    let t1C_5s = tail t1Cs

    let t1_4 = tail t1_5
    let t1C_4 = tail t1C_5
    let t1_4s = tail t1_5s
    let t1C_4s = tail t1C_5s

    let t1_3 = tail t1_4
    let t1C_3 = tail t1C_4
    let t1_3s = tail t1_4s
    let t1C_3s = tail t1C_4s

    let t1_2 = tail t1_3
    let t1C_2 = tail t1C_3
    let t1_2s = tail t1_3s
    let t1C_2s = tail t1C_3s

    let t1_1 = tail t1_2
    let t1C_1 = tail t1C_2
    let t1_1s = tail t1_2s
    let t1C_1s = tail t1C_2s

    (((length t1) = 6) && ((length t1C) = 6) && ((length t1s) = 6) && ((length t1Cs) = 6)
    && ((head t1) = "f") && ((head t1C) = "f") && ((head t1s) = "f") && ((head t1Cs) = "f")
    && ((length t1_5) = 5) && ((length t1C_5) = 5) && ((length t1_5s) = 5) && ((length t1C_5s) = 5)
    && ((head t1_5) = "e") && ((head t1C_5) = "e") && ((head t1_5s) = "e") && ((head t1C_5s) = "e")
    && ((length t1_4) = 4) && ((length t1C_4) = 4) && ((length t1_4s) = 4) && ((length t1C_4s) = 4)
    && ((head t1_4) = "d") && ((head t1C_4) = "d") && ((head t1_4s) = "d") && ((head t1C_4s) = "d")
    && ((length t1_3) = 3) && ((length t1C_3) = 3) && ((length t1_3s) = 3) && ((length t1C_3s) = 3)
    && ((head t1_3) = "c") && ((head t1C_3) = "c") && ((head t1_3s) = "c") && ((head t1C_3s) = "c")
    && ((length t1_2) = 2) && ((length t1C_2) = 2) && ((length t1_2s) = 2) && ((length t1C_2s) = 2) 
    && ((head t1_2) = "b") && ((head t1C_2) = "b") && ((head t1_2s) = "b") && ((head t1C_2s) = "b")
    && ((length t1_1) = 1) && ((length t1C_1) = 1) && ((length t1_1s) = 1) && ((length t1C_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1C_1) = "a") && ((head t1_1s) = "a") && ((head t1C_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 7``() =
    let t1 = tail len8
    let t1C = tail len8C3
    let t1s = tail len8snoc
    let t1Cs = tail len8C3snoc

    let t1_6 = tail t1
    let t1C_6 = tail t1C
    let t1_6s = tail t1s
    let t1C_6s = tail t1Cs

    let t1_5 = tail t1_6
    let t1C_5 = tail t1C_6
    let t1_5s = tail t1_6s
    let t1C_5s = tail t1C_6s

    let t1_4 = tail t1_5
    let t1C_4 = tail t1C_5
    let t1_4s = tail t1_5s
    let t1C_4s = tail t1C_5s

    let t1_3 = tail t1_4
    let t1C_3 = tail t1C_4
    let t1_3s = tail t1_4s
    let t1C_3s = tail t1C_4s

    let t1_2 = tail t1_3
    let t1C_2 = tail t1C_3
    let t1_2s = tail t1_3s
    let t1C_2s = tail t1C_3s

    let t1_1 = tail t1_2
    let t1C_1 = tail t1C_2
    let t1_1s = tail t1_2s
    let t1C_1s = tail t1C_2s

    (((length t1) = 7) && ((length t1C) = 7) && ((length t1s) = 7) && ((length t1Cs) = 7)
    && ((head t1) = "g") && ((head t1C) = "g") && ((head t1s) = "g") && ((head t1Cs) = "g")
    && ((length t1_6) = 6) && ((length t1C_6) = 6) && ((length t1_6s) = 6) && ((length t1C_6s) = 6)
    && ((head t1_6) = "f") && ((head t1C_6) = "f") && ((head t1_6s) = "f") && ((head t1C_6s) = "f")
    && ((length t1_5) = 5) && ((length t1C_5) = 5) && ((length t1_5s) = 5) && ((length t1C_5s) = 5)
    && ((head t1_5) = "e") && ((head t1C_5) = "e") && ((head t1_5s) = "e") && ((head t1C_5s) = "e")
    && ((length t1_4) = 4) && ((length t1C_4) = 4) && ((length t1_4s) = 4) && ((length t1C_4s) = 4)
    && ((head t1_4) = "d") && ((head t1C_4) = "d") && ((head t1_4s) = "d") && ((head t1C_4s) = "d")
    && ((length t1_3) = 3) && ((length t1C_3) = 3) && ((length t1_3s) = 3) && ((length t1C_3s) = 3)
    && ((head t1_3) = "c") && ((head t1C_3) = "c") && ((head t1_3s) = "c") && ((head t1C_3s) = "c")
    && ((length t1_2) = 2) && ((length t1C_2) = 2) && ((length t1_2s) = 2) && ((length t1C_2s) = 2) 
    && ((head t1_2) = "b") && ((head t1C_2) = "b") && ((head t1_2s) = "b") && ((head t1C_2s) = "b")
    && ((length t1_1) = 1) && ((length t1C_1) = 1) && ((length t1_1s) = 1) && ((length t1C_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1C_1) = "a") && ((head t1_1s) = "a") && ((head t1C_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 8``() =
    let t1 = tail len9
    let t1C = tail len9C3
    let t1s = tail len9snoc
    let t1Cs = tail len9C3snoc

    let t1_7 = tail t1
    let t1C_7 = tail t1C
    let t1_7s = tail t1s
    let t1C_7s = tail t1Cs

    let t1_6 = tail t1_7
    let t1C_6 = tail t1C_7
    let t1_6s = tail t1_7s
    let t1C_6s = tail t1C_7s

    let t1_5 = tail t1_6
    let t1C_5 = tail t1C_6
    let t1_5s = tail t1_6s
    let t1C_5s = tail t1C_6s

    let t1_4 = tail t1_5
    let t1C_4 = tail t1C_5
    let t1_4s = tail t1_5s
    let t1C_4s = tail t1C_5s

    let t1_3 = tail t1_4
    let t1C_3 = tail t1C_4
    let t1_3s = tail t1_4s
    let t1C_3s = tail t1C_4s

    let t1_2 = tail t1_3
    let t1C_2 = tail t1C_3
    let t1_2s = tail t1_3s
    let t1C_2s = tail t1C_3s

    let t1_1 = tail t1_2
    let t1C_1 = tail t1C_2
    let t1_1s = tail t1_2s
    let t1C_1s = tail t1C_2s

    (((length t1) = 8) && ((length t1C) = 8) && ((length t1s) = 8) && ((length t1Cs) = 8)
    && ((head t1) = "h") && ((head t1C) = "h") && ((head t1s) = "h") && ((head t1Cs) = "h")
    && ((length t1_7) = 7) && ((length t1C_7) = 7) && ((length t1_7s) = 7) && ((length t1C_7s) = 7)
    && ((head t1_7) = "g") && ((head t1C_7) = "g") && ((head t1_7s) = "g") && ((head t1C_7s) = "g")
    && ((length t1_6) = 6) && ((length t1C_6) = 6) && ((length t1_6s) = 6) && ((length t1C_6s) = 6)
    && ((head t1_6) = "f") && ((head t1C_6) = "f") && ((head t1_6s) = "f") && ((head t1C_6s) = "f")
    && ((length t1_5) = 5) && ((length t1C_5) = 5) && ((length t1_5s) = 5) && ((length t1C_5s) = 5)
    && ((head t1_5) = "e") && ((head t1C_5) = "e") && ((head t1_5s) = "e") && ((head t1C_5s) = "e")
    && ((length t1_4) = 4) && ((length t1C_4) = 4) && ((length t1_4s) = 4) && ((length t1C_4s) = 4)
    && ((head t1_4) = "d") && ((head t1C_4) = "d") && ((head t1_4s) = "d") && ((head t1C_4s) = "d")
    && ((length t1_3) = 3) && ((length t1C_3) = 3) && ((length t1_3s) = 3) && ((length t1C_3s) = 3)
    && ((head t1_3) = "c") && ((head t1C_3) = "c") && ((head t1_3s) = "c") && ((head t1C_3s) = "c")
    && ((length t1_2) = 2) && ((length t1C_2) = 2) && ((length t1_2s) = 2) && ((length t1C_2s) = 2) 
    && ((head t1_2) = "b") && ((head t1C_2) = "b") && ((head t1_2s) = "b") && ((head t1C_2s) = "b")
    && ((length t1_1) = 1) && ((length t1C_1) = 1) && ((length t1_1s) = 1) && ((length t1C_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1C_1) = "a") && ((head t1_1s) = "a") && ((head t1C_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 9``() =
    let t1 = tail lena
    let t1C = tail lenaC3
    let t1s = tail lenasnoc
    let t1Cs = tail lenaC3snoc

    let t1_8 = tail t1
    let t1C_8 = tail t1C
    let t1_8s = tail t1s
    let t1C_8s = tail t1Cs

    let t1_7 = tail t1_8
    let t1C_7 = tail t1C_8
    let t1_7s = tail t1_8s
    let t1C_7s = tail t1C_8s

    let t1_6 = tail t1_7
    let t1C_6 = tail t1C_7
    let t1_6s = tail t1_7s
    let t1C_6s = tail t1C_7s

    let t1_5 = tail t1_6
    let t1C_5 = tail t1C_6
    let t1_5s = tail t1_6s
    let t1C_5s = tail t1C_6s

    let t1_4 = tail t1_5
    let t1C_4 = tail t1C_5
    let t1_4s = tail t1_5s
    let t1C_4s = tail t1C_5s

    let t1_3 = tail t1_4
    let t1C_3 = tail t1C_4
    let t1_3s = tail t1_4s
    let t1C_3s = tail t1C_4s

    let t1_2 = tail t1_3
    let t1C_2 = tail t1C_3
    let t1_2s = tail t1_3s
    let t1C_2s = tail t1C_3s

    let t1_1 = tail t1_2
    let t1C_1 = tail t1C_2
    let t1_1s = tail t1_2s
    let t1C_1s = tail t1C_2s

    (((length t1) = 9) && ((length t1C) = 9) && ((length t1s) = 9) && ((length t1Cs) = 9)
    && ((head t1) = "i") && ((head t1C) = "i") && ((head t1s) = "i") && ((head t1Cs) = "i")
    && ((length t1_8) = 8) && ((length t1C_8) = 8) && ((length t1_8s) = 8) && ((length t1C_8s) = 8)
    && ((head t1_8) = "h") && ((head t1C_8) = "h") && ((head t1_8s) = "h") && ((head t1C_8s) = "h")
    && ((length t1_7) = 7) && ((length t1C_7) = 7) && ((length t1_7s) = 7) && ((length t1C_7s) = 7)
    && ((head t1_7) = "g") && ((head t1C_7) = "g") && ((head t1_7s) = "g") && ((head t1C_7s) = "g")
    && ((length t1_6) = 6) && ((length t1C_6) = 6) && ((length t1_6s) = 6) && ((length t1C_6s) = 6)
    && ((head t1_6) = "f") && ((head t1C_6) = "f") && ((head t1_6s) = "f") && ((head t1C_6s) = "f")
    && ((length t1_5) = 5) && ((length t1C_5) = 5) && ((length t1_5s) = 5) && ((length t1C_5s) = 5)
    && ((head t1_5) = "e") && ((head t1C_5) = "e") && ((head t1_5s) = "e") && ((head t1C_5s) = "e")
    && ((length t1_4) = 4) && ((length t1C_4) = 4) && ((length t1_4s) = 4) && ((length t1C_4s) = 4)
    && ((head t1_4) = "d") && ((head t1C_4) = "d") && ((head t1_4s) = "d") && ((head t1C_4s) = "d")
    && ((length t1_3) = 3) && ((length t1C_3) = 3) && ((length t1_3s) = 3) && ((length t1C_3s) = 3)
    && ((head t1_3) = "c") && ((head t1C_3) = "c") && ((head t1_3s) = "c") && ((head t1C_3s) = "c")
    && ((length t1_2) = 2) && ((length t1C_2) = 2) && ((length t1_2s) = 2) && ((length t1C_2s) = 2) 
    && ((head t1_2) = "b") && ((head t1C_2) = "b") && ((head t1_2s) = "b") && ((head t1C_2s) = "b")
    && ((length t1_1) = 1) && ((length t1C_1) = 1) && ((length t1_1s) = 1) && ((length t1C_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1C_1) = "a") && ((head t1_1s) = "a") && ((head t1C_1s) = "a")) |> should equal true

[<Test>]
//the previous series thoroughly tested construction by snoc, so we'll leave those out
let ``last, init, and length work test 1``() =  
    let t1 = init len2
    let t1C = init len2C3
    
    (((length t1) = 1) && ((length t1C) = 1) && ((last t1) = "b") && ((last t1C) = "b")) |> should equal true

[<Test>]
let ``last, init, and length work test 2``() =
    let t1 = init len3
    let t1C = init len3C3
    
    let t1_1 = init t1
    let t1C_1 = init t1C
    
    (((length t1) = 2) && ((length t1C) = 2) && ((last t1) = "b") && ((last t1C) = "b") 
    && ((length t1_1) = 1) && ((length t1C_1) = 1) && ((last t1_1) = "c") && ((last t1C_1) = "c")) |> should equal true

[<Test>]
let ``last, init, and length work test 3``() =
    let t1 = init len4
    let t1C = init len4C3
    let t1_1 = init t1
    let t1C_1 = init t1C
    let t1_2 = init t1_1
    let t1C_2 = init t1C_1
    
    (((length t1) = 3) && ((length t1C) = 3) && ((last t1) = "b") && ((last t1C) = "b") 
    && ((length t1_1) = 2) && ((length t1C_1) = 2) && ((last t1_1) = "c") && ((last t1C_1) = "c")
    && ((length t1_2) = 1) && ((length t1C_2) = 1) && ((last t1_2) = "d") && ((last t1C_2) = "d")) |> should equal true

[<Test>]
let ``last, init, and length work test 4``() =
    let t1 = init len5
    let t1C = init len5C3
    let t1_1 = init t1
    let t1C_1 = init t1C
    let t1_2 = init t1_1
    let t1C_2 = init t1C_1
    let t1_3 = init t1_2
    let t1C_3 = init t1C_2
    
    (((length t1) = 4) && ((length t1C) = 4) && ((last t1) = "b") && ((last t1C) = "b") 
    && ((length t1_1) = 3) && ((length t1C_1) = 3) && ((last t1_1) = "c") && ((last t1C_1) = "c")
    && ((length t1_2) = 2) && ((length t1C_2) = 2) && ((last t1_2) = "d") && ((last t1C_2) = "d")
    && ((length t1_3) = 1) && ((length t1C_3) = 1) && ((last t1_3) = "e") && ((last t1C_3) = "e")) |> should equal true

[<Test>]
let ``last, init, and length work test 5``() =
    let t1 = init len6
    let t1C = init len6C3
    let t1_1 = init t1
    let t1C_1 = init t1C
    let t1_2 = init t1_1
    let t1C_2 = init t1C_1
    let t1_3 = init t1_2
    let t1C_3 = init t1C_2
    let t1_4 = init t1_3
    let t1C_4 = init t1C_3
    
    (((length t1) = 5) && ((length t1C) = 5) && ((last t1) = "b") && ((last t1C) = "b") 
    && ((length t1_1) = 4) && ((length t1C_1) = 4) && ((last t1_1) = "c") && ((last t1C_1) = "c")
    && ((length t1_2) = 3) && ((length t1C_2) = 3) && ((last t1_2) = "d") && ((last t1C_2) = "d")
    && ((length t1_3) = 2) && ((length t1C_3) = 2) && ((last t1_3) = "e") && ((last t1C_3) = "e")
    && ((length t1_4) = 1) && ((length t1C_4) = 1) && ((last t1_4) = "f") && ((last t1C_4) = "f")) |> should equal true

[<Test>]
let ``last, init, and length work test 6``() =
    let t1 = init len7
    let t1C = init len7C3
    let t1_1 = init t1
    let t1C_1 = init t1C
    let t1_2 = init t1_1
    let t1C_2 = init t1C_1
    let t1_3 = init t1_2
    let t1C_3 = init t1C_2
    let t1_4 = init t1_3
    let t1C_4 = init t1C_3
    let t1_5 = init t1_4
    let t1C_5 = init t1C_4
    
    (((length t1) = 6) && ((length t1C) = 6) && ((last t1) = "b") && ((last t1C) = "b") 
    && ((length t1_1) = 5) && ((length t1C_1) = 5) && ((last t1_1) = "c") && ((last t1C_1) = "c")
    && ((length t1_2) = 4) && ((length t1C_2) = 4) && ((last t1_2) = "d") && ((last t1C_2) = "d")
    && ((length t1_3) = 3) && ((length t1C_3) = 3) && ((last t1_3) = "e") && ((last t1C_3) = "e")
    && ((length t1_4) = 2) && ((length t1C_4) = 2) && ((last t1_4) = "f") && ((last t1C_4) = "f")
    && ((length t1_5) = 1) && ((length t1C_5) = 1) && ((last t1_5) = "g") && ((last t1C_5) = "g")) |> should equal true

[<Test>]
let ``last, init, and length work test 7``() =
    let t1 = init len8
    let t1C = init len8C3
    let t1_1 = init t1
    let t1C_1 = init t1C
    let t1_2 = init t1_1
    let t1C_2 = init t1C_1
    let t1_3 = init t1_2
    let t1C_3 = init t1C_2
    let t1_4 = init t1_3
    let t1C_4 = init t1C_3
    let t1_5 = init t1_4
    let t1C_5 = init t1C_4
    let t1_6 = init t1_5
    let t1C_6 = init t1C_5
    
    (((length t1) = 7) && ((length t1C) = 7) && ((last t1) = "b") && ((last t1C) = "b") 
    && ((length t1_1) = 6) && ((length t1C_1) = 6) && ((last t1_1) = "c") && ((last t1C_1) = "c")
    && ((length t1_2) = 5) && ((length t1C_2) = 5) && ((last t1_2) = "d") && ((last t1C_2) = "d")
    && ((length t1_3) = 4) && ((length t1C_3) = 4) && ((last t1_3) = "e") && ((last t1C_3) = "e")
    && ((length t1_4) = 3) && ((length t1C_4) = 3) && ((last t1_4) = "f") && ((last t1C_4) = "f")
    && ((length t1_5) = 2) && ((length t1C_5) = 2) && ((last t1_5) = "g") && ((last t1C_5) = "g")
    && ((length t1_6) = 1) && ((length t1C_6) = 1) && ((last t1_6) = "h") && ((last t1C_6) = "h")) |> should equal true

[<Test>]
let ``last, init, and length work test 8``() =
    let t1 = init len9
    let t1C = init len9C3
    let t1_1 = init t1
    let t1C_1 = init t1C
    let t1_2 = init t1_1
    let t1C_2 = init t1C_1
    let t1_3 = init t1_2
    let t1C_3 = init t1C_2
    let t1_4 = init t1_3
    let t1C_4 = init t1C_3
    let t1_5 = init t1_4
    let t1C_5 = init t1C_4
    let t1_6 = init t1_5
    let t1C_6 = init t1C_5
    let t1_7 = init t1_6
    let t1C_7 = init t1C_6
    
    (((length t1) = 8) && ((length t1C) = 8) && ((last t1) = "b") && ((last t1C) = "b") 
    && ((length t1_1) = 7) && ((length t1C_1) = 7) && ((last t1_1) = "c") && ((last t1C_1) = "c")
    && ((length t1_2) = 6) && ((length t1C_2) = 6) && ((last t1_2) = "d") && ((last t1C_2) = "d")
    && ((length t1_3) = 5) && ((length t1C_3) = 5) && ((last t1_3) = "e") && ((last t1C_3) = "e")
    && ((length t1_4) = 4) && ((length t1C_4) = 4) && ((last t1_4) = "f") && ((last t1C_4) = "f")
    && ((length t1_5) = 3) && ((length t1C_5) = 3) && ((last t1_5) = "g") && ((last t1C_5) = "g")
    && ((length t1_6) = 2) && ((length t1C_6) = 2) && ((last t1_6) = "h") && ((last t1C_6) = "h")
    && ((length t1_7) = 1) && ((length t1C_7) = 1) && ((last t1_7) = "i") && ((last t1C_7) = "i")) |> should equal true

[<Test>]
let ``last, init, and length work test 9``() =
    let t1 = init lena
    let t1C = init lenaC3
    let t1_1 = init t1
    let t1C_1 = init t1C
    let t1_2 = init t1_1
    let t1C_2 = init t1C_1
    let t1_3 = init t1_2
    let t1C_3 = init t1C_2
    let t1_4 = init t1_3
    let t1C_4 = init t1C_3
    let t1_5 = init t1_4
    let t1C_5 = init t1C_4
    let t1_6 = init t1_5
    let t1C_6 = init t1C_5
    let t1_7 = init t1_6
    let t1C_7 = init t1C_6
    let t1_8 = init t1_7
    let t1C_8 = init t1C_7
    
    (((length t1) = 9) && ((length t1C) = 9) && ((last t1) = "b") && ((last t1C) = "b") 
    && ((length t1_1) = 8) && ((length t1C_1) = 8) && ((last t1_1) = "c") && ((last t1C_1) = "c")
    && ((length t1_2) = 7) && ((length t1C_2) = 7) && ((last t1_2) = "d") && ((last t1C_2) = "d")
    && ((length t1_3) = 6) && ((length t1C_3) = 6) && ((last t1_3) = "e") && ((last t1C_3) = "e")
    && ((length t1_4) = 5) && ((length t1C_4) = 5) && ((last t1_4) = "f") && ((last t1C_4) = "f")
    && ((length t1_5) = 4) && ((length t1C_5) = 4) && ((last t1_5) = "g") && ((last t1C_5) = "g")
    && ((length t1_6) = 3) && ((length t1C_6) = 3) && ((last t1_6) = "h") && ((last t1C_6) = "h")
    && ((length t1_7) = 2) && ((length t1C_7) = 2) && ((last t1_7) = "i") && ((last t1C_7) = "i")
    && ((length t1_8) = 1) && ((length t1C_8) = 1) && ((last t1_8) = "j") && ((last t1C_8) = "j")) |> should equal true

[<Test>]
let ``IEnumerable Seq nth``() =
    lena |> Seq.nth 5 |> should equal "e"

[<Test>]
let ``IEnumerable Seq length``() =
    lena |> Seq.length |> should equal 10

[<Test>]
let ``type cons works``() =
    lena.Cons "zz" |> head |> should equal "zz"

[<Test>]
let ``IDeque cons works``() =
    ((lena :> IDeque<string>).Cons "zz").Head |> should equal "zz"

[<Test>]
let ``ofCatLists and uncons``() =
    let d = ofCatLists ["a";"b";"c"] ["d";"e";"f"]
    let h1, t1 = uncons d
    let h2, t2 = uncons t1
    let h3, t3 = uncons t2
    let h4, t4 = uncons t3
    let h5, t5 = uncons t4
    let h6, t6 = uncons t5

    ((h1 = "a") && (h2 = "b") && (h3 = "c") && (h4 = "d") && (h5 = "e") && (h6 = "f") && (isEmpty t6)) |> should equal true

[<Test>]
let ``ofCatSeqs and uncons``() =
    let d = ofCatSeqs (seq {'a'..'c'}) (seq {'d'..'f'})
    let h1, t1 = uncons d
    let h2, t2 = uncons t1
    let h3, t3 = uncons t2
    let h4, t4 = uncons t3
    let h5, t5 = uncons t4
    let h6, t6 = uncons t5

    ((h1 = 'a') && (h2 = 'b') && (h3 = 'c') && (h4 = 'd') && (h5 = 'e') && (h6 = 'f') && (isEmpty t6)) |> should equal true

[<Test>]
let ``unsnoc works``() =
    let d = ofCatLists ["f";"e";"d"] ["c";"b";"a"]
    let i1, l1 = unsnoc d
    let i2, l2 = unsnoc i1
    let i3, l3 = unsnoc i2
    let i4, l4 = unsnoc i3
    let i5, l5 = unsnoc i4
    let i6, l6 = unsnoc i5

    ((l1 = "a") && (l2 = "b") && (l3 = "c") && (l4 = "d") && (l5 = "e") && (l6 = "f") && (isEmpty i6)) |> should equal true

[<Test>]
let ``snoc pattern discriminator``() =
    let d = (ofCatLists ["f";"e";"d"] ["c";"b";"a"]) 
    let i1, l1 = unsnoc d 

    let i2, l2 = 
        match i1 with
        | Snoc(i, l) -> i, l
        | _ -> i1, "x"

    ((l2 = "b") && ((length i2) = 4)) |> should equal true

[<Test>]
let ``cons pattern discriminator``() =
    let d = (ofCatLists ["f";"e";"d"] ["c";"b";"a"]) 
    let h1, t1 = uncons d 

    let h2, t2 = 
        match t1 with
        | Cons(h, t) -> h, t
        | _ ->  "x", t1

    ((h2 = "e") && ((length t2) = 4)) |> should equal true

[<Test>]
let ``cons and snoc pattern discriminator``() =
    let d = (ofCatLists ["f";"e";"d"] ["c";"b";"a"]) 
    
    let mid1 = 
        match d with
        | Cons(h, Snoc(i, l)) -> i
        | _ -> d

    let head, last = 
        match mid1 with
        | Cons(h, Snoc(i, l)) -> h, l
        | _ -> "x", "x"

    ((head = "e") && (last = "b")) |> should equal true

[<Test>]
let ``rev empty dqueue should be empty``() =
    isEmpty (rev (empty 2)) |> should equal true

[<Test>]
let ``rev dqueue length 1``() =
    ((head (rev len1) = "a") && (head (rev len1C3) = "a")) |> should equal true

[<Test>]
let ``rev dqueue length 2``() =
    let r1 = rev len2
    let r1c = rev len2C3
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")) |> should equal true

[<Test>]
let ``rev dqueue length 3``() =
    let r1 = rev len3
    let r1c = rev len3C3
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b") && (h3 = "c") && (h3c = "c")) |> should equal true

[<Test>]
let ``rev dqueue length 4``() =
    let r1 = rev len4
    let r1c = rev len4C3
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b") && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d")) |> should equal true

[<Test>]
let ``rev dqueue length 5``() =
    let r1 = rev len5
    let r1c = rev len5C3
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c
    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b") && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d")
    && (h5 = "e") && (h5c = "e")) |> should equal true

[<Test>]
//length 6 more than sufficient to test rev
let ``rev dqueue length 6``() =
    let r1 = rev len6
    let r1c = rev len6C3
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c
    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c
    let t6 = tail t5
    let t6c = tail t5c
    let h6 = head t6
    let h6c = head t6c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b") && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d")
    && (h5 = "e") && (h5c = "e") && (h6 = "f") && (h6c = "f")) |> should equal true

[<Test>]
let ``ofSeq and ofSeqC empty``() =
    ((isEmpty (ofSeq [])) && (isEmpty (ofSeqC 3 []))) |> should equal true

[<Test>]
let ``ofSeq and ofSeqC length 1``() =
    ((head       (ofSeq ["a"])      = "a") && (head      (ofSeqC 3 ["a"])      = "a")) |> should equal true

[<Test>]
let ``ofSeq and ofSeqC length 2``() =
    let r1 = ofSeq ["a";"b"]
    let r1c = ofSeqC 3 ["a";"b"]
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")) |> should equal true

[<Test>]
let ``ofSeq and ofSeqC length 3``() =
    let r1 = ofSeq ["a";"b";"c"]
    let r1c = ofSeqC 3 ["a";"b";"c"]
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b") && (h3 = "c") && (h3c = "c")) |> should equal true

[<Test>]
let ``ofSeq and ofSeqC length 4``() =
    let r1 = ofSeq ["a";"b";"c";"d"]
    let r1c = ofSeqC 3 ["a";"b";"c";"d"]
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b") && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d")) |> should equal true

[<Test>]
//length 5 more than sufficient to test ofSeq and ofSeqC
let ``ofSeq and ofSeqC length 5``() =
    let r1 = ofSeq ["a";"b";"c";"d";"e"]
    let r1c = ofSeqC 3 ["a";"b";"c";"d";"e"]
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c
    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b") && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d")
    && (h5 = "e") && (h5c = "e")) |> should equal true

[<Test>]
//length 5 more than sufficient to test ofSeq and ofSeqC
let ``ofSeq and ofSeqC length 6``() =
    let r1 = ofSeq ["a";"b";"c";"d";"e";"f"]
    let r1c = ofSeqC 3 ["a";"b";"c";"d";"e";"f"]
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c
    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c
    let t6 = tail t5
    let t6c = tail t5c
    let h6 = head t6
    let h6c = head t6c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b") && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d")
    && (h5 = "e") && (h5c = "e") && (h6 = "f") && (h6c = "f")) |> should equal true

[<Test>]
let ``appending empty dqueus``() =
    ((isEmpty (append (ofSeq []) (ofSeq []) )) && (isEmpty (append (empty 3) (empty 3)))
    ) |> should equal true

[<Test>]
let ``appending empty and length 1``() =
    ((head (append (ofSeq []) len1) = "a") && (head (append len1 (empty 3)) = "a")) |> should equal true

[<Test>]
let ``appending empty and length 2``() =
    let r1 = append (ofSeq []) (ofSeq ["a";"b"])
    let r1c = append (empty 3) (ofSeqC 3 ["a";"b"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c

    let r1r = append (ofSeq ["a";"b"]) (ofSeq [])
    let r1cr = append (ofSeqC 3 ["a";"b"]) (empty 3)
    let h1r = head r1r
    let h1cr = head r1cr
    let t2r = tail r1r
    let t2cr = tail r1cr
    let h2r = head t2r
    let h2cr = head t2cr

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")
    && (h1r = "a") && (h1cr = "a") && (h2r = "b") && (h2cr = "b")) |> should equal true

[<Test>]
let ``appending length 1 and length 2``() =
    let r1 = append (ofSeq ["a"]) (ofSeq ["b";"c"])
    let r1c = append (ofSeqC 3 ["a"]) (ofSeqC 3 ["b";"c"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c

    let r1r = append (ofSeq ["a";"b"]) (ofSeq ["c"])
    let r1cr = append (ofSeqC 3 ["a";"b"]) (ofSeqC 3 ["c"])
    let h1r = head r1r
    let h1cr = head r1cr
    let t2r = tail r1r
    let t2cr = tail r1cr
    let h2r = head t2r
    let h2cr = head t2cr
    let t3r = tail t2r
    let t3cr = tail t2cr
    let h3r = head t3r
    let h3cr = head t3cr

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c")
    && (h1r = "a") && (h1cr = "a") && (h2r = "b") && (h2cr = "b") && (h3r = "c") && (h3cr = "c")) |> should equal true

[<Test>]
let ``appending length 1 and length 3``() =
    let r1 = append (ofSeq ["a"]) (ofSeq ["b";"c";"d"])
    let r1c = append (ofSeqC 3 ["a"]) (ofSeqC 3 ["b";"c";"d"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c

    let r1r = append (ofSeq ["a";"b";"c"]) (ofSeq ["d"])
    let r1cr = append (ofSeqC 3 ["a";"b";"c"]) (ofSeqC 3 ["d"])
    let h1r = head r1r
    let h1cr = head r1cr
    let t2r = tail r1r
    let t2cr = tail r1cr
    let h2r = head t2r
    let h2cr = head t2cr
    let t3r = tail t2r
    let t3cr = tail t2cr
    let h3r = head t3r
    let h3cr = head t3cr
    let t4r = tail t3r
    let t4cr = tail t3cr
    let h4r = head t4r
    let h4cr = head t4cr

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d")
    && (h1r = "a") && (h1cr = "a") && (h2r = "b") && (h2cr = "b") && (h3r = "c") && (h3cr = "c") && (h4r = "d") && (h4cr = "d")) |> should equal true

[<Test>]
let ``appending length 1 and length 4``() =
    let r1 = append (ofSeq ["a"]) (ofSeq ["b";"c";"d";"e"])
    let r1c = append (ofSeqC 3 ["a"]) (ofSeqC 3 ["b";"c";"d";"e"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c

    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c

    let r1r = append (ofSeq ["a";"b";"c";"d"]) (ofSeq ["e"])
    let r1cr = append (ofSeqC 3 ["a";"b";"c";"d"]) (ofSeqC 3 ["e"])
    let h1r = head r1r
    let h1cr = head r1cr
    let t2r = tail r1r
    let t2cr = tail r1cr
    let h2r = head t2r
    let h2cr = head t2cr
    let t3r = tail t2r
    let t3cr = tail t2cr
    let h3r = head t3r
    let h3cr = head t3cr
    let t4r = tail t3r
    let t4cr = tail t3cr
    let h4r = head t4r
    let h4cr = head t4cr

    let t5r = tail t4r
    let t5cr = tail t4cr
    let h5r = head t5r
    let h5cr = head t5cr

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d") 
    && (h5 = "e") && (h5c = "e")
    && (h1r = "a") && (h1cr = "a") && (h2r = "b") && (h2cr = "b") && (h3r = "c") && (h3cr = "c") && (h4r = "d") && (h4cr = "d") 
    && (h5r = "e") && (h5cr = "e")) |> should equal true

[<Test>]
let ``appending length 1 and length 5``() =
    let r1 = append (ofSeq ["a"]) (ofSeq ["b";"c";"d";"e";"f"])
    let r1c = append (ofSeqC 3 ["a"]) (ofSeqC 3 ["b";"c";"d";"e";"f"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c
    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c
    let t6 = tail t5
    let t6c = tail t5c
    let h6 = head t6
    let h6c = head t6c

    let r1r = append (ofSeq ["a";"b";"c";"d";"e"]) (ofSeq ["f"])
    let r1cr = append (ofSeqC 3 ["a";"b";"c";"d";"e"]) (ofSeqC 3 ["f"])
    let h1r = head r1r
    let h1cr = head r1cr
    let t2r = tail r1r
    let t2cr = tail r1cr
    let h2r = head t2r
    let h2cr = head t2cr
    let t3r = tail t2r
    let t3cr = tail t2cr
    let h3r = head t3r
    let h3cr = head t3cr
    let t4r = tail t3r
    let t4cr = tail t3cr
    let h4r = head t4r
    let h4cr = head t4cr
    let t5r = tail t4r
    let t5cr = tail t4cr
    let h5r = head t5r
    let h5cr = head t5cr
    let t6r = tail t5r
    let t6cr = tail t5cr
    let h6r = head t6r
    let h6cr = head t6cr

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d") 
    && (h5 = "e") && (h5c = "e") && (h6 = "f") && (h6c = "f")
    && (h1r = "a") && (h1cr = "a") && (h2r = "b") && (h2cr = "b") && (h3r = "c") && (h3cr = "c") && (h4r = "d") && (h4cr = "d") 
    && (h5r = "e") && (h5cr = "e") && (h6r = "f") && (h6cr = "f")) |> should equal true

[<Test>]
let ``appending length 6 and length 7``() =
    let r1 = append (ofSeq ["a";"b";"c";"d";"e";"f"]) (ofSeq ["g";"h";"i";"j";"k";"l";"m"])
    let r1c = append (ofSeqC 3 ["a";"b";"c";"d";"e";"f"]) (ofSeqC 3 ["g";"h";"i";"j";"k";"l";"m"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c
    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c
    let t6 = tail t5
    let t6c = tail t5c
    let h6 = head t6
    let h6c = head t6c
    let h6 = head t6
    let h6c = head t6c
    let t7 = tail t6
    let t7c = tail t6c
    let h7 = head t7
    let h7c = head t7c
    let h7 = head t7
    let h7c = head t7c
    let t8 = tail t7
    let t8c = tail t7c
    let h8 = head t8
    let h8c = head t8c
    let h8 = head t8
    let h8c = head t8c
    let t9 = tail t8
    let t9c = tail t8c
    let h9 = head t9
    let h9c = head t9c
    let h9 = head t9
    let h9c = head t9c
    let t10 = tail t9
    let t10c = tail t9c
    let h10 = head t10
    let h10c = head t10c
    let h10 = head t10
    let h10c = head t10c
    let t11 = tail t10
    let t11c = tail t10c
    let h11 = head t11
    let h11c = head t11c
    let h11 = head t11
    let h11c = head t11c
    let t12 = tail t11
    let t12c = tail t11c
    let h12 = head t12
    let h12c = head t12c
    let h12 = head t12
    let h12c = head t12c
    let t13 = tail t12
    let t13c = tail t12c
    let h13 = head t13
    let h13c = head t13c
    let h13 = head t13
    let h13c = head t13c

    let r1r = append (ofSeq ["a";"b";"c";"d";"e";"f";"g"]) (ofSeq ["h";"i";"j";"k";"l";"m"])
    let r1cr = append (ofSeqC 3 ["a";"b";"c";"d";"e";"f";"g"]) (ofSeqC 3 ["h";"i";"j";"k";"l";"m"])
    let h1r = head r1r
    let h1cr = head r1cr
    let t2r = tail r1r
    let t2cr = tail r1cr
    let h2r = head t2r
    let h2cr = head t2cr
    let t3r = tail t2r
    let t3cr = tail t2cr
    let h3r = head t3r
    let h3cr = head t3cr
    let t4r = tail t3r
    let t4cr = tail t3cr
    let h4r = head t4r
    let h4cr = head t4cr
    let t5r = tail t4r
    let t5cr = tail t4cr
    let h5r = head t5r
    let h5cr = head t5cr
    let t6r = tail t5r
    let t6cr = tail t5cr
    let h6r = head t6r
    let h6cr = head t6cr
    let t7r = tail t6r
    let t7cr = tail t6cr
    let h7r = head t7r
    let h7cr = head t7cr
    let h7r = head t7r
    let h7cr = head t7cr
    let t8r = tail t7r
    let t8cr = tail t7cr
    let h8r = head t8r
    let h8cr = head t8cr
    let h8r = head t8r
    let h8cr = head t8cr
    let t9r = tail t8r
    let t9cr = tail t8cr
    let h9r = head t9r
    let h9cr = head t9cr
    let h9r = head t9r
    let h9cr = head t9cr
    let t10r = tail t9r
    let t10cr = tail t9cr
    let h10r = head t10r
    let h10cr = head t10cr
    let h10r = head t10r
    let h10cr = head t10cr
    let t11r = tail t10r
    let t11cr = tail t10cr
    let h11r = head t11r
    let h11cr = head t11cr
    let h11r = head t11r
    let h11cr = head t11cr
    let t12r = tail t11r
    let t12cr = tail t11cr
    let h12r = head t12r
    let h12cr = head t12cr
    let h12r = head t12r
    let h12cr = head t12cr
    let t13r = tail t12r
    let t13cr = tail t12cr
    let h13r = head t13r
    let h13cr = head t13cr
    let h13r = head t13r
    let h13cr = head t13cr

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d") 
    && (h5 = "e") && (h5c = "e") && (h6 = "f") && (h6c = "f") && (h7 = "g") && (h7c = "g") && (h8 = "h") && (h8c = "h") 
    && (h9 = "i") && (h9c = "i") && (h10 = "j") && (h10c = "j") && (h11 = "k") && (h11c = "k") && (h12 = "l") && (h12c = "l") 
    && (h13 = "m") && (h13c = "m") && (h1r = "a") && (h1cr = "a") && (h2r = "b") && (h2cr = "b") && (h3r = "c") && (h3cr = "c") 
    && (h4r = "d") && (h4cr = "d") && (h5r = "e") && (h5cr = "e") && (h6r = "f") && (h6cr = "f") && (h7r = "g") && (h7cr = "g") 
    && (h8r = "h") && (h8cr = "h") && (h9r = "i") && (h9cr = "i") && (h10r = "j") && (h10cr = "j") 
    && (h11r = "k") && (h11cr = "k") && (h12r = "l") && (h12cr = "l") && (h13r = "m") && (h13cr = "m")) |> should equal true

[<Test>]
let ``appending length 2 and length 2``() =
    let r1 = append (ofSeq ["a";"b"]) (ofSeq ["c";"d"])
    let r1c = append (ofSeqC 3 ["a";"b"]) (ofSeqC 3 ["c";"d"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d")) |> should equal true

[<Test>]
let ``appending length 2 and length 3``() =
    let r1 = append (ofSeq ["a";"b"]) (ofSeq ["c";"d";"e"])
    let r1c = append (ofSeqC 3 ["a";"b"]) (ofSeqC 3 ["c";"d";"e"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c

    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c

    let r1r = append (ofSeq ["a";"b";"c"]) (ofSeq ["d";"e"])
    let r1cr = append (ofSeqC 3 ["a";"b";"c"]) (ofSeqC 3 ["d";"e"])
    let h1r = head r1r
    let h1cr = head r1cr
    let t2r = tail r1r
    let t2cr = tail r1cr
    let h2r = head t2r
    let h2cr = head t2cr
    let t3r = tail t2r
    let t3cr = tail t2cr
    let h3r = head t3r
    let h3cr = head t3cr
    let t4r = tail t3r
    let t4cr = tail t3cr
    let h4r = head t4r
    let h4cr = head t4cr

    let t5r = tail t4r
    let t5cr = tail t4cr
    let h5r = head t5r
    let h5cr = head t5cr

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d") 
    && (h5 = "e") && (h5c = "e")
    && (h1r = "a") && (h1cr = "a") && (h2r = "b") && (h2cr = "b") && (h3r = "c") && (h3cr = "c") && (h4r = "d") && (h4cr = "d") 
    && (h5r = "e") && (h5cr = "e")) |> should equal true

[<Test>]
let ``appending length 2 and length 4``() =
    let r1 = append (ofSeq ["a";"b"]) (ofSeq ["c";"d";"e";"f"])
    let r1c = append (ofSeqC 3 ["a";"b"]) (ofSeqC 3 ["c";"d";"e";"f"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c
    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c
    let t6 = tail t5
    let t6c = tail t5c
    let h6 = head t6
    let h6c = head t6c

    let r1r = append (ofSeq ["a";"b";"c";"d"]) (ofSeq ["e";"f"])
    let r1cr = append (ofSeqC 3 ["a";"b";"c";"d"]) (ofSeqC 3 ["e";"f"])
    let h1r = head r1r
    let h1cr = head r1cr
    let t2r = tail r1r
    let t2cr = tail r1cr
    let h2r = head t2r
    let h2cr = head t2cr
    let t3r = tail t2r
    let t3cr = tail t2cr
    let h3r = head t3r
    let h3cr = head t3cr
    let t4r = tail t3r
    let t4cr = tail t3cr
    let h4r = head t4r
    let h4cr = head t4cr
    let t5r = tail t4r
    let t5cr = tail t4cr
    let h5r = head t5r
    let h5cr = head t5cr
    let t6r = tail t5r
    let t6cr = tail t5cr
    let h6r = head t6r
    let h6cr = head t6cr

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d") 
    && (h5 = "e") && (h5c = "e") && (h6 = "f") && (h6c = "f")
    && (h1r = "a") && (h1cr = "a") && (h2r = "b") && (h2cr = "b") && (h3r = "c") && (h3cr = "c") && (h4r = "d") && (h4cr = "d") 
    && (h5r = "e") && (h5cr = "e") && (h6r = "f") && (h6cr = "f")) |> should equal true

[<Test>]
let ``appending length 2 and length 5``() =
    let r1 = append (ofSeq ["a";"b"]) (ofSeq ["c";"d";"e";"f";"g"])
    let r1c = append (ofSeqC 3 ["a";"b"]) (ofSeqC 3 ["c";"d";"e";"f";"g"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c
    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c
    let t6 = tail t5
    let t6c = tail t5c
    let h6 = head t6
    let h6c = head t6c
    let t7 = tail t6
    let t7c = tail t6c
    let h7 = head t7
    let h7c = head t7c

    let r1r = append (ofSeq ["a";"b";"c";"d";"e"]) (ofSeq ["f";"g"])
    let r1cr = append (ofSeqC 3 ["a";"b";"c";"d";"e"]) (ofSeqC 3 ["f";"g"])
    let h1r = head r1r
    let h1cr = head r1cr
    let t2r = tail r1r
    let t2cr = tail r1cr
    let h2r = head t2r
    let h2cr = head t2cr
    let t3r = tail t2r
    let t3cr = tail t2cr
    let h3r = head t3r
    let h3cr = head t3cr
    let t4r = tail t3r
    let t4cr = tail t3cr
    let h4r = head t4r
    let h4cr = head t4cr
    let t5r = tail t4r
    let t5cr = tail t4cr
    let h5r = head t5r
    let h5cr = head t5cr
    let t6r = tail t5r
    let t6cr = tail t5cr
    let h6r = head t6r
    let h6cr = head t6cr
    let t7r = tail t6r
    let t7cr = tail t6cr
    let h7r = head t7r
    let h7cr = head t7cr

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d") 
    && (h5 = "e") && (h5c = "e") && (h6 = "f") && (h6c = "f") && (h7 = "g") && (h7c = "g")
    && (h1r = "a") && (h1cr = "a") && (h2r = "b") && (h2cr = "b") && (h3r = "c") && (h3cr = "c") && (h4r = "d") && (h4cr = "d") 
    && (h5r = "e") && (h5cr = "e") && (h6r = "f") && (h6cr = "f") && (h7r = "g") && (h7cr = "g")) |> should equal true

[<Test>]
let ``appending length 3 and length 3``() =
    let r1 = append (ofSeq ["a";"b";"c"]) (ofSeq ["d";"e";"f"])
    let r1c = append (ofSeqC 3 ["a";"b";"c"]) (ofSeqC 3 ["d";"e";"f"])
    let h1 = head r1
    let h1c = head r1c
    let t2 = tail r1
    let t2c = tail r1c
    let h2 = head t2
    let h2c = head t2c
    let t3 = tail t2
    let t3c = tail t2c
    let h3 = head t3
    let h3c = head t3c
    let t4 = tail t3
    let t4c = tail t3c
    let h4 = head t4
    let h4c = head t4c
    let t5 = tail t4
    let t5c = tail t4c
    let h5 = head t5
    let h5c = head t5c
    let t6 = tail t5
    let t6c = tail t5c
    let h6 = head t6
    let h6c = head t6c

    ((h1 = "a") && (h1c = "a") && (h2 = "b") && (h2c = "b")  && (h3 = "c") && (h3c = "c") && (h4 = "d") && (h4c = "d") 
    && (h5 = "e") && (h5c = "e") && (h6 = "f") && (h6c = "f")) |> should equal true


[<Test>]
let ``lookup length 1``() =
    len1 |> lookup 0 |> should equal "a"

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
let ``remove elements length 1``() =
    len1 |> remove 0 |> isEmpty |> should equal true

[<Test>]
let ``remove elements length 2``() =
    let a = len2 |> remove 0 |> head 
    let b = len2 |> remove 1 |> head
    ((a = "a") && (b = "b")) |> should equal true

[<Test>]
let ``remove elements length 3``() =
    let r0 = (ofSeq ["a";"b";"c"]) |> remove 0
    let b0 = head r0
    let t0 = tail r0
    let c0 = head t0

    let r1 = (ofSeq ["a";"b";"c"]) |> remove 1
    let a1 = head r1
    let t1 = tail r1
    let c1 = head t1

    let r2 = (ofSeq ["a";"b";"c"]) |> remove 2
    let a2 = head r2
    let t2 = tail r2
    let b2 = head t2

    ((b0 = "b") && (c0 = "c") && (a1 = "a") && (c1 = "c") && (a2 = "a") && (b2 = "b")) |> should equal true

[<Test>]
let ``remove elements length 4``() =
    let r0 = (ofSeq ["a";"b";"c";"d"]) |> remove 0
    let b0 = head r0
    let t0 = tail r0
    let c0 = head t0
    let t01 = tail t0
    let d0 = head t01

    let r1 = (ofSeq ["a";"b";"c";"d"]) |> remove 1
    let a1 = head r1
    let t11 = tail r1
    let c1 = head t11
    let t12 = tail t11
    let d1 = head t12

    let r2 = (ofSeq ["a";"b";"c";"d"]) |> remove 2
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22

    let r3 = (ofSeq ["a";"b";"c";"d"]) |> remove 3
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32

    ((b0 = "b") && (c0 = "c") && (d0 = "d") 
    && (a1 = "a") && (c1 = "c") && (d1 = "d")
    && (a2 = "a") && (b2 = "b") && (c2 = "d")
    && (a3 = "a") && (b3 = "b") && (c3 = "c")) |> should equal true

[<Test>]
let ``remove elements length 5``() =
    let r0 = (ofSeq ["a";"b";"c";"d";"e"]) |> remove 0
    let b0 = head r0
    let t01 = tail r0
    let c0 = head t01
    let t02= tail t01
    let d0 = head t02
    let t03 = tail t02
    let e0 = head t03

    let r1 = (ofSeq ["a";"b";"c";"d";"e"]) |> remove 1
    let a1 = head r1
    let t11 = tail r1
    let c1 = head t11
    let t12 = tail t11
    let d1 = head t12
    let t13 = tail t12
    let e1 = head t13

    let r2 = (ofSeq ["a";"b";"c";"d";"e"]) |> remove 2
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22
    let t23 = tail t22
    let e2 = head t23

    let r3 = (ofSeq ["a";"b";"c";"d";"e"]) |> remove 3
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32
    let t33 = tail t32
    let e3 = head t33

    let r4 = (ofSeq ["a";"b";"c";"d";"e"]) |> remove 4
    let a4 = head r4
    let t41 = tail r4
    let b4 = head t41
    let t42 = tail t41
    let c4 = head t42
    let t43 = tail t42
    let d4 = head t43

    ((b0 = "b") && (c0 = "c") && (d0 = "d") && (e0 = "e")
    && (a1 = "a") && (c1 = "c") && (d1 = "d") && (e1 = "e")
    && (a2 = "a") && (b2 = "b") && (c2 = "d") && (e2 = "e")
    && (a3 = "a") && (b3 = "b") && (c3 = "c") && (e3 = "e")
    && (a4 = "a") && (b4 = "b") && (c4 = "c") && (d4 = "d")) |> should equal true

[<Test>]
let ``remove elements length 6``() =
    let r0 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> remove 0
    let b0 = head r0
    let t01 = tail r0
    let c0 = head t01
    let t02= tail t01
    let d0 = head t02
    let t03 = tail t02
    let e0 = head t03
    let t04 = tail t03
    let f0 = head t04

    let r1 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> remove 1
    let a1 = head r1
    let t11 = tail r1
    let c1 = head t11
    let t12 = tail t11
    let d1 = head t12
    let t13 = tail t12
    let e1 = head t13
    let t14 = tail t13
    let f1 = head t14

    let r2 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> remove 2
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22
    let t23 = tail t22
    let e2 = head t23
    let t24 = tail t23
    let f2 = head t24

    let r3 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> remove 3
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32
    let t33 = tail t32
    let e3 = head t33
    let t34 = tail t33
    let f3 = head t34

    let r4 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> remove 4
    let a4 = head r4
    let t41 = tail r4
    let b4 = head t41
    let t42 = tail t41
    let c4 = head t42
    let t43 = tail t42
    let d4 = head t43
    let t44 = tail t43
    let f4 = head t44

    let r5 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> remove 5
    let a5 = head r5
    let t51 = tail r5
    let b5 = head t51
    let t52 = tail t51
    let c5 = head t52
    let t53 = tail t52
    let d5 = head t53
    let t54 = tail t53
    let e5 = head t54

    ((b0 = "b") && (c0 = "c") && (d0 = "d") && (e0 = "e") && (f0 = "f")
    && (a1 = "a") && (c1 = "c") && (d1 = "d") && (e1 = "e") && (f1 = "f")
    && (a2 = "a") && (b2 = "b") && (c2 = "d") && (e2 = "e") && (f2 = "f")
    && (a3 = "a") && (b3 = "b") && (c3 = "c") && (e3 = "e") && (f3 = "f")
    && (a4 = "a") && (b4 = "b") && (c4 = "c") && (d4 = "d") && (f4 = "f")
    && (a5 = "a") && (b5 = "b") && (c5 = "c") && (d5 = "d") && (e5 = "e")) |> should equal true

[<Test>]
let ``tryRemoveempty``() =
    (empty 3) |> tryRemove 0 |> should equal None

[<Test>]
let ``tryRemove elements length 1``() =
    let a = len1 |> tryRemove 0 
    a.Value |> isEmpty |> should equal true

[<Test>]
let ``tryRemove elements length 2``() =
    let a = len2 |> tryRemove 0 
    let a1 =  head a.Value
    let b = len2 |> tryRemove 1 
    let b1 = head b.Value
    ((a1 = "a") && (b1 = "b")) |> should equal true

[<Test>]
let ``tryRemove elements length 3``() =
    let x0 = (ofSeq ["a";"b";"c"]) |> tryRemove 0
    let r0 = x0.Value
    let b0 = head r0
    let t0 = tail r0
    let c0 = head t0

    let x1 = (ofSeq ["a";"b";"c"]) |> tryRemove 1
    let r1 = x1.Value
    let a1 = head r1
    let t1 = tail r1
    let c1 = head t1

    let x2 = (ofSeq ["a";"b";"c"]) |> tryRemove 2
    let r2 = x2.Value
    let a2 = head r2
    let t2 = tail r2
    let b2 = head t2

    ((b0 = "b") && (c0 = "c") && (a1 = "a") && (c1 = "c") && (a2 = "a") && (b2 = "b")) |> should equal true

[<Test>]
let ``tryRemove elements length 4``() =
    let x0 = (ofSeq ["a";"b";"c";"d"]) |> tryRemove 0
    let r0 = x0.Value
    let b0 = head r0
    let t0 = tail r0
    let c0 = head t0
    let t01 = tail t0
    let d0 = head t01

    let x1 = (ofSeq ["a";"b";"c";"d"]) |> tryRemove 1
    let r1 = x1.Value
    let a1 = head r1
    let t11 = tail r1
    let c1 = head t11
    let t12 = tail t11
    let d1 = head t12
 
    let x2 = (ofSeq ["a";"b";"c";"d"]) |> tryRemove 2
    let r2 = x2.Value
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22
     
    let x3 = (ofSeq ["a";"b";"c";"d"]) |> tryRemove 3
    let r3 = x3.Value
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32

    ((b0 = "b") && (c0 = "c") && (d0 = "d") 
    && (a1 = "a") && (c1 = "c") && (d1 = "d")
    && (a2 = "a") && (b2 = "b") && (c2 = "d")
    && (a3 = "a") && (b3 = "b") && (c3 = "c")) |> should equal true

[<Test>]
let ``tryRemove elements length 5``() =
    let x0 = (ofSeq ["a";"b";"c";"d";"e"]) |> tryRemove 0
    let r0 = x0.Value
    let b0 = head r0
    let t01 = tail r0
    let c0 = head t01
    let t02= tail t01
    let d0 = head t02
    let t03 = tail t02
    let e0 = head t03

    let x1 = (ofSeq ["a";"b";"c";"d";"e"]) |> tryRemove 1
    let r1 = x1.Value
    let a1 = head r1
    let t11 = tail r1
    let c1 = head t11
    let t12 = tail t11
    let d1 = head t12
    let t13 = tail t12
    let e1 = head t13

    let x2 = (ofSeq ["a";"b";"c";"d";"e"]) |> tryRemove 2
    let r2 = x2.Value
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22
    let t23 = tail t22
    let e2 = head t23

    let x3 = (ofSeq ["a";"b";"c";"d";"e"]) |> tryRemove 3
    let r3 = x3.Value
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32
    let t33 = tail t32
    let e3 = head t33

    let x4 = (ofSeq ["a";"b";"c";"d";"e"]) |> tryRemove 4
    let r4 = x4.Value
    let a4 = head r4
    let t41 = tail r4
    let b4 = head t41
    let t42 = tail t41
    let c4 = head t42
    let t43 = tail t42
    let d4 = head t43

    ((b0 = "b") && (c0 = "c") && (d0 = "d") && (e0 = "e")
    && (a1 = "a") && (c1 = "c") && (d1 = "d") && (e1 = "e")
    && (a2 = "a") && (b2 = "b") && (c2 = "d") && (e2 = "e")
    && (a3 = "a") && (b3 = "b") && (c3 = "c") && (e3 = "e")
    && (a4 = "a") && (b4 = "b") && (c4 = "c") && (d4 = "d")) |> should equal true

[<Test>]
let ``tryRemove elements length 6``() =
    let x0 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> tryRemove 0
    let r0 = x0.Value
    let b0 = head r0
    let t01 = tail r0
    let c0 = head t01
    let t02= tail t01
    let d0 = head t02
    let t03 = tail t02
    let e0 = head t03
    let t04 = tail t03
    let f0 = head t04

    let x1 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> tryRemove 1
    let r1 =x1.Value
    let a1 = head r1
    let t11 = tail r1
    let c1 = head t11
    let t12 = tail t11
    let d1 = head t12
    let t13 = tail t12
    let e1 = head t13
    let t14 = tail t13
    let f1 = head t14

    let x2 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> tryRemove 2
    let r2 = x2.Value 
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22
    let t23 = tail t22
    let e2 = head t23
    let t24 = tail t23
    let f2 = head t24

    let x3 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> tryRemove 3
    let r3 = x3.Value
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32
    let t33 = tail t32
    let e3 = head t33
    let t34 = tail t33
    let f3 = head t34

    let x4 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> tryRemove 4
    let r4 = x4.Value
    let a4 = head r4
    let t41 = tail r4
    let b4 = head t41
    let t42 = tail t41
    let c4 = head t42
    let t43 = tail t42
    let d4 = head t43
    let t44 = tail t43
    let f4 = head t44

    let x5 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> tryRemove 5
    let r5 = x5.Value
    let a5 = head r5
    let t51 = tail r5
    let b5 = head t51
    let t52 = tail t51
    let c5 = head t52
    let t53 = tail t52
    let d5 = head t53
    let t54 = tail t53
    let e5 = head t54

    ((b0 = "b") && (c0 = "c") && (d0 = "d") && (e0 = "e") && (f0 = "f")
    && (a1 = "a") && (c1 = "c") && (d1 = "d") && (e1 = "e") && (f1 = "f")
    && (a2 = "a") && (b2 = "b") && (c2 = "d") && (e2 = "e") && (f2 = "f")
    && (a3 = "a") && (b3 = "b") && (c3 = "c") && (e3 = "e") && (f3 = "f")
    && (a4 = "a") && (b4 = "b") && (c4 = "c") && (d4 = "d") && (f4 = "f")
    && (a5 = "a") && (b5 = "b") && (c5 = "c") && (d5 = "d") && (e5 = "e")) |> should equal true

[<Test>]
let ``update elements length 1``() =
    len1 |> update 0 "aa" |> head |> should equal "aa"

[<Test>]
let ``update elements length 2``() =
    let r0 = (ofSeq ["a";"b"]) |> update 0 "zz"
    let a0 = head r0
    let t01 = tail r0
    let b0 = head t01

    let r1 = (ofSeq ["a";"b"]) |> update 1 "zz"
    let a1 = head r1
    let t11 = tail r1
    let b1 = head t11

    ((a0 = "zz") && (b0 = "b")  
    && (a1 = "a") && (b1 = "zz")) |> should equal true

[<Test>]
let ``update elements length 3``() =
    let r0 = (ofSeq ["a";"b";"c"]) |> update 0 "zz"
    let a0 = head r0
    let t01 = tail r0
    let b0 = head t01
    let t02 = tail t01
    let c0 = head t02

    let r1 = (ofSeq ["a";"b";"c"]) |> update 1 "zz"
    let a1 = head r1
    let t11 = tail r1
    let b1 = head t11
    let t12 = tail t11
    let c1 = head t12

    let r2 = (ofSeq ["a";"b";"c"]) |> update 2 "zz"
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22

    ((a0 = "zz") && (b0 = "b") && (c0 = "c") 
    && (a1 = "a") && (b1 = "zz") && (c1 = "c") 
    && (a2 = "a") && (b2 = "b") && (c2 = "zz")) |> should equal true

[<Test>]
let ``update elements length 4``() =
    let r0 = (ofSeq ["a";"b";"c";"d"]) |> update 0 "zz"
    let a0 = head r0
    let t01 = tail r0
    let b0 = head t01
    let t02 = tail t01
    let c0 = head t02
    let t03 = tail t02
    let d0 = head t03

    let r1 = (ofSeq ["a";"b";"c";"d"]) |> update 1 "zz"
    let a1 = head r1
    let t11 = tail r1
    let b1 = head t11
    let t12 = tail t11
    let c1 = head t12
    let t13 = tail t12
    let d1 = head t13

    let r2 = (ofSeq ["a";"b";"c";"d"]) |> update 2 "zz"
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22
    let t23 = tail t22
    let d2 = head t23

    let r3 = (ofSeq ["a";"b";"c";"d"]) |> update 3 "zz"
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32
    let t33 = tail t32
    let d3 = head t33

    ((a0 = "zz") && (b0 = "b") && (c0 = "c") && (d0 = "d") 
    && (a1 = "a") && (b1 = "zz") && (c1 = "c") && (d1 = "d") 
    && (a2 = "a") && (b2 = "b") && (c2 = "zz") && (d2 = "d") 
    && (a3 = "a") && (b3 = "b") && (c3 = "c") && (d3 = "zz")) |> should equal true

[<Test>]
let ``update elements length 5``() =
    let r0 = (ofSeq ["a";"b";"c";"d";"e"]) |> update 0 "zz"
    let a0 = head r0
    let t01 = tail r0
    let b0 = head t01
    let t02 = tail t01
    let c0 = head t02
    let t03 = tail t02
    let d0 = head t03
    let t04 = tail t03
    let e0 = head t04

    let r1 = (ofSeq ["a";"b";"c";"d";"e"]) |> update 1 "zz"
    let a1 = head r1
    let t11 = tail r1
    let b1 = head t11
    let t12 = tail t11
    let c1 = head t12
    let t13 = tail t12
    let d1 = head t13
    let t14 = tail t13
    let e1 = head t14

    let r2 = (ofSeq ["a";"b";"c";"d";"e"]) |> update 2 "zz"
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22
    let t23 = tail t22
    let d2 = head t23
    let t24 = tail t23
    let e2 = head t24

    let r3 = (ofSeq ["a";"b";"c";"d";"e"]) |> update 3 "zz"
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32
    let t33 = tail t32
    let d3 = head t33
    let t34 = tail t33
    let e3 = head t34

    let r4 = (ofSeq ["a";"b";"c";"d";"e"]) |> update 4 "zz"
    let a4 = head r4
    let t41 = tail r4
    let b4 = head t41
    let t42 = tail t41
    let c4 = head t42
    let t43 = tail t42
    let d4 = head t43
    let t44 = tail t43
    let e4 = head t44

    ((a0 = "zz") && (b0 = "b") && (c0 = "c") && (d0 = "d")  && (e0 = "e")
    && (a1 = "a") && (b1 = "zz") && (c1 = "c") && (d1 = "d") && (e1 = "e") 
    && (a2 = "a") && (b2 = "b") && (c2 = "zz") && (d2 = "d") && (e2 = "e") 
    && (a3 = "a") && (b3 = "b") && (c3 = "c") && (d3 = "zz")  && (e3 = "e")
    && (a4 = "a") && (b4 = "b") && (c4 = "c") && (d4 = "d")  && (e4 = "zz")) |> should equal true

[<Test>]
let ``update elements length 6``() =
    let r0 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> update 0 "zz"
    let a0 = head r0
    let t01 = tail r0
    let b0 = head t01
    let t02 = tail t01
    let c0 = head t02
    let t03 = tail t02
    let d0 = head t03
    let t04 = tail t03
    let e0 = head t04
    let t05 = tail t04
    let f0 = head t05

    let r1 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> update 1 "zz"
    let a1 = head r1
    let t11 = tail r1
    let b1 = head t11
    let t12 = tail t11
    let c1 = head t12
    let t13 = tail t12
    let d1 = head t13
    let t14 = tail t13
    let e1 = head t14
    let t15 = tail t14
    let f1 = head t15

    let r2 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> update 2 "zz"
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22
    let t23 = tail t22
    let d2 = head t23
    let t24 = tail t23
    let e2 = head t24
    let t25 = tail t24
    let f2 = head t25

    let r3 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> update 3 "zz"
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32
    let t33 = tail t32
    let d3 = head t33
    let t34 = tail t33
    let e3 = head t34
    let t35 = tail t34
    let f3 = head t35

    let r4 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> update 4 "zz"
    let a4 = head r4
    let t41 = tail r4
    let b4 = head t41
    let t42 = tail t41
    let c4 = head t42
    let t43 = tail t42
    let d4 = head t43
    let t44 = tail t43
    let e4 = head t44
    let t45 = tail t44
    let f4 = head t45

    let r5 = (ofSeq ["a";"b";"c";"d";"e";"f"]) |> update 5 "zz"
    let a5 = head r5
    let t51 = tail r5
    let b5 = head t51
    let t52 = tail t51
    let c5 = head t52
    let t53 = tail t52
    let d5 = head t53
    let t54 = tail t53
    let e5 = head t54
    let t55 = tail t54
    let f5 = head t55

    ((a0 = "zz") && (b0 = "b") && (c0 = "c") && (d0 = "d")  && (e0 = "e") && (f0 = "f")
    && (a1 = "a") && (b1 = "zz") && (c1 = "c") && (d1 = "d") && (e1 = "e") && (f1 = "f")
    && (a2 = "a") && (b2 = "b") && (c2 = "zz") && (d2 = "d") && (e2 = "e") && (f2 = "f")
    && (a3 = "a") && (b3 = "b") && (c3 = "c") && (d3 = "zz")  && (e3 = "e") && (f3 = "f")
    && (a4 = "a") && (b4 = "b") && (c4 = "c") && (d4 = "d")  && (e4 = "zz") && (f4 = "f")
    && (a5 = "a") && (b5 = "b") && (c5 = "c") && (d5 = "d")  && (e5 = "e") && (f5 = "zz")) |> should equal true

[<Test>]
let ``tryUpdate elements length 1``() =
    let a = len1 |> tryUpdate 0 "aa" 
    a.Value |> head |> should equal "aa"

[<Test>]
let ``tryUpdate elements length 2``() =
    let x0 = (ofSeq ["a";"b"]) |> tryUpdate 0 "zz"
    let r0 = x0.Value
    let a0 = head r0
    let t01 = tail r0
    let b0 = head t01

    let x1 = (ofSeq ["a";"b"]) |> tryUpdate 1 "zz"
    let r1 = x1.Value
    let a1 = head r1
    let t11 = tail r1
    let b1 = head t11

    ((a0 = "zz") && (b0 = "b")  
    && (a1 = "a") && (b1 = "zz")) |> should equal true

[<Test>]
let ``tryUpdate elements length 3``() =
    let x0 = (ofSeq ["a";"b";"c"]) |> tryUpdate 0 "zz"
    let r0 = x0.Value
    let a0 = head r0
    let t01 = tail r0
    let b0 = head t01
    let t02 = tail t01
    let c0 = head t02

    let x1 = (ofSeq ["a";"b";"c"]) |> tryUpdate 1 "zz"
    let r1 = x1.Value
    let a1 = head r1
    let t11 = tail r1
    let b1 = head t11
    let t12 = tail t11
    let c1 = head t12

    let x2 = (ofSeq ["a";"b";"c"]) |> tryUpdate 2 "zz"
    let r2 = x2.Value
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22

    ((a0 = "zz") && (b0 = "b") && (c0 = "c") 
    && (a1 = "a") && (b1 = "zz") && (c1 = "c") 
    && (a2 = "a") && (b2 = "b") && (c2 = "zz")) |> should equal true

[<Test>]
let ``tryUpdate elements length 4``() =
    let x0 = (ofSeq ["a";"b";"c";"d"]) |> tryUpdate 0 "zz"
    let r0 = x0.Value
    let a0 = head r0
    let t01 = tail r0
    let b0 = head t01
    let t02 = tail t01
    let c0 = head t02
    let t03 = tail t02
    let d0 = head t03

    let x1 = (ofSeq ["a";"b";"c";"d"]) |> tryUpdate 1 "zz"
    let r1 = x1.Value
    let a1 = head r1
    let t11 = tail r1
    let b1 = head t11
    let t12 = tail t11
    let c1 = head t12
    let t13 = tail t12
    let d1 = head t13

    let x2 = (ofSeq ["a";"b";"c";"d"]) |> tryUpdate 2 "zz"
    let r2 = x2.Value
    let a2 = head r2
    let t21 = tail r2
    let b2 = head t21
    let t22 = tail t21
    let c2 = head t22
    let t23 = tail t22
    let d2 = head t23

    let x3 = (ofSeq ["a";"b";"c";"d"]) |> tryUpdate 3 "zz"
    let r3 = x3.Value
    let a3 = head r3
    let t31 = tail r3
    let b3 = head t31
    let t32 = tail t31
    let c3 = head t32
    let t33 = tail t32
    let d3 = head t33

    ((a0 = "zz") && (b0 = "b") && (c0 = "c") && (d0 = "d") 
    && (a1 = "a") && (b1 = "zz") && (c1 = "c") && (d1 = "d") 
    && (a2 = "a") && (b2 = "b") && (c2 = "zz") && (d2 = "d") 
    && (a3 = "a") && (b3 = "b") && (c3 = "c") && (d3 = "zz")) |> should equal true

[<Test>]
let ``tryUncons on empty``() =
    let q = empty 2
    (tryUncons q = None) |> should equal true

[<Test>]
let ``tryUncons on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    let x, xs = (tryUncons q).Value 
    x |> should equal "a"

[<Test>]
let ``tryUnsnoc on empty``() =
    let q = empty 2
    (tryUnsnoc q = None) |> should equal true

[<Test>]
let ``tryUnsnoc on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    let xs, x = (tryUnsnoc q).Value 
    x |> should equal "d"

[<Test>]
let ``tryGetHead on empty``() =
    let q = empty 2
    (tryGetHead q = None) |> should equal true

[<Test>]
let ``tryGetHead on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    (tryGetHead q).Value |> should equal "a"

[<Test>]
let ``tryGetInit on empty``() =
    let q = empty 2
    (tryGetInit q = None) |> should equal true

[<Test>]
let ``tryGetInit on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
//    (tryGetInit q).Value |> last |> should equal "c"
    let x = (tryGetInit q).Value 
    let x2 = x|> last 
    x2 |> should equal "c"

[<Test>]
let ``tryGetLast on empty``() =
    let q = empty 2
    (tryGetLast q = None) |> should equal true

[<Test>]
let ``tryGetLast on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    (tryGetLast q).Value |> should equal "d"


[<Test>]
let ``tryGetTail on empty``() =
    let q = empty 2
    (tryGetTail q = None) |> should equal true

[<Test>]
let ``tryGetTail on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    (tryGetTail q).Value |> head |> should equal "b"