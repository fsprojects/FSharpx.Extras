module FSharpx.DataStructures.Tests.Deque

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.Deque
open NUnit.Framework
open FsUnit


//quite a lot going on and difficult to reason about edge cases
//testing up to length of 6 is the likely minimum to satisfy any arbitrary test case (less for some cases)

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

let len2snoc = singleton "b" |> snoc "a"
let len3snoc = singleton "c" |> snoc "b" |> snoc "a"
let len4snoc = singleton "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len5snoc = singleton "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len6snoc = singleton "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len7snoc = singleton "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len8snoc = singleton "h" |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let len9snoc = singleton "i" |> snoc "h" |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"
let lenasnoc = singleton "j" |> snoc "i" |> snoc "h" |> snoc "g" |> snoc "f" |> snoc "e" |> snoc "d" |> snoc "c" |> snoc "b" |> snoc "a"

[<Test>]
let ``empty dqueue should be empty``() =
    empty() |> isEmpty |> should equal true

[<Test>]
let ``cons works``() =
    len2 |> isEmpty |> should equal false

[<Test>]
let ``snoc works``() =
    len2snoc |> isEmpty |> should equal false

[<Test>]
let ``singleton head works``() =
    len1 |> head |> should equal "a"

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
    len1 |> init |> isEmpty |> should equal true

[<Test>]
let ``head, tail, and length work test 1``() =
    let t1 = tail len2
    let t1s = tail len2snoc
    (((length t1) = 1) && ((length t1s) = 1) && ((head t1) = "a") && ((head t1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 2``() =
    let t1 = tail len3
    let t1s = tail len3snoc

    let t1_1 = tail t1
    let t1_1s = tail t1s

    (((length t1) = 2) && ((length t1s) = 2) && ((head t1) = "b") && ((head t1s) = "b") && ((length t1_1) = 1) && ((length t1_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 3``() =
    let t1 = tail len4
    let t1s = tail len4snoc

    let t1_2 = tail t1
    let t1_2s = tail t1s

    let t1_1 = tail t1_2
    let t1_1s = tail t1_2s

    (((length t1) = 3) && ((length t1s) = 3) && ((head t1) = "c")&& ((head t1s) = "c") && ((length t1_2) = 2) && ((length t1_2s) = 2)
    && ((head t1_2) = "b") && ((head t1_2s) = "b") && ((length t1_1) = 1) && ((length t1_1s) = 1) && ((head t1_1) = "a") 
    && ((head t1_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 4``() =
    let t1 = tail len5
    let t1s = tail len5snoc

    let t1_3 = tail t1
    let t1_3s = tail t1s

    let t1_2 = tail t1_3
    let t1_2s = tail t1_3s

    let t1_1 = tail t1_2
    let t1_1s = tail t1_2s

    (((length t1) = 4) && ((length t1s) = 4) && ((head t1) = "d") && ((head t1s) = "d") && ((length t1_3) = 3) && ((length t1_3s) = 3)
    && ((head t1_3) = "c") && ((head t1_3s) = "c") && ((length t1_2) = 2) && ((length t1_2s) = 2) && ((head t1_2) = "b") && ((head t1_2s) = "b")
    && ((length t1_1) = 1) && ((length t1_1s) = 1) && ((head t1_1) = "a") && ((head t1_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 5``() =
    let t1 = tail len6
    let t1s = tail len6snoc

    let t1_4 = tail t1
    let t1_4s = tail t1s

    let t1_3 = tail t1_4
    let t1_3s = tail t1_4s

    let t1_2 = tail t1_3
    let t1_2s = tail t1_3s

    let t1_1 = tail t1_2
    let t1_1s = tail t1_2s

    (((length t1) = 5) && ((length t1s) = 5) && ((head t1) = "e") && ((head t1s) = "e") && ((length t1_4) = 4) && ((length t1_4s) = 4) 
    && ((head t1_4) = "d") && ((head t1_4s) = "d") && ((length t1_3) = 3) && ((length t1_3s) = 3) && ((head t1_3) = "c") && ((head t1_3s) = "c") 
    && ((length t1_2) = 2) && ((length t1_2s) = 2) && ((head t1_2) = "b") && ((head t1_2s) = "b") && ((length t1_1) = 1) && ((length t1_1s) = 1)
    && ((head t1_1) = "a") && ((head t1_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 6``() =
    let t1 = tail len7
    let t1s = tail len7snoc

    let t1_5 = tail t1
    let t1_5s = tail t1s

    let t1_4 = tail t1_5
    let t1_4s = tail t1_5s

    let t1_3 = tail t1_4
    let t1_3s = tail t1_4s

    let t1_2 = tail t1_3
    let t1_2s = tail t1_3s

    let t1_1 = tail t1_2
    let t1_1s = tail t1_2s

    (((length t1) = 6) && ((length t1s) = 6)  
    && ((head t1) = "f") && ((head t1s) = "f") 
    && ((length t1_5) = 5) && ((length t1_5s) = 5) 
    && ((head t1_5) = "e") && ((head t1_5s) = "e") 
    && ((length t1_4) = 4) && ((length t1_4s) = 4) 
    && ((head t1_4) = "d") && ((head t1_4s) = "d") 
    && ((length t1_3) = 3) && ((length t1_3s) = 3) 
    && ((head t1_3) = "c") && ((head t1_3s) = "c") 
    && ((length t1_2) = 2) && ((length t1_2s) = 2) 
    && ((head t1_2) = "b") && ((head t1_2s) = "b") 
    && ((length t1_1) = 1) && ((length t1_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1_1s) = "a") ) |> should equal true

[<Test>]
let ``head, tail, and length work test 7``() =
    let t1 = tail len8
    let t1s = tail len8snoc
    let t1_6 = tail t1
    let t1_6s = tail t1s
    let t1_5 = tail t1_6
    let t1_5s = tail t1_6s
    let t1_4 = tail t1_5
    let t1_4s = tail t1_5s
    let t1_3 = tail t1_4
    let t1_3s = tail t1_4s
    let t1_2 = tail t1_3
    let t1_2s = tail t1_3s
    let t1_1 = tail t1_2
    let t1_1s = tail t1_2s

    (((length t1) = 7) && ((length t1s) = 7) 
    && ((head t1) = "g") && ((head t1s) = "g") 
    && ((length t1_6) = 6)  && ((length t1_6s) = 6) 
    && ((head t1_6) = "f")  && ((head t1_6s) = "f") 
    && ((length t1_5) = 5)  && ((length t1_5s) = 5) 
    && ((head t1_5) = "e")  && ((head t1_5s) = "e") 
    && ((length t1_4) = 4)  && ((length t1_4s) = 4) 
    && ((head t1_4) = "d")  && ((head t1_4s) = "d") 
    && ((length t1_3) = 3)  && ((length t1_3s) = 3) 
    && ((head t1_3) = "c")  && ((head t1_3s) = "c") 
    && ((length t1_2) = 2)  && ((length t1_2s) = 2) 
    && ((head t1_2) = "b")  && ((head t1_2s) = "b") 
    && ((length t1_1) = 1)  && ((length t1_1s) = 1) 
    && ((head t1_1) = "a")  && ((head t1_1s) = "a") ) |> should equal true

[<Test>]
let ``head, tail, and length work test 8``() =
    let t1 = tail len9
    let t1s = tail len9snoc
    let t1_7 = tail t1
    let t1_7s = tail t1s  
    let t1_6 = tail t1_7
    let t1_6s = tail t1_7s
    let t1_5 = tail t1_6
    let t1_5s = tail t1_6s
    let t1_4 = tail t1_5
    let t1_4s = tail t1_5s
    let t1_3 = tail t1_4
    let t1_3s = tail t1_4s
    let t1_2 = tail t1_3
    let t1_2s = tail t1_3s
    let t1_1 = tail t1_2
    let t1_1s = tail t1_2s

    (((length t1) = 8) && ((length t1s) = 8) 
    && ((head t1) = "h") && ((head t1s) = "h") 
    && ((length t1_7) = 7)  && ((length t1_7s) = 7) 
    && ((head t1_7) = "g")  && ((head t1_7s) = "g") 
    && ((length t1_6) = 6)  && ((length t1_6s) = 6) 
    && ((head t1_6) = "f")  && ((head t1_6s) = "f") 
    && ((length t1_5) = 5)  && ((length t1_5s) = 5) 
    && ((head t1_5) = "e")  && ((head t1_5s) = "e") 
    && ((length t1_4) = 4)  && ((length t1_4s) = 4) 
    && ((head t1_4) = "d")  && ((head t1_4s) = "d") 
    && ((length t1_3) = 3)  && ((length t1_3s) = 3) 
    && ((head t1_3) = "c")  && ((head t1_3s) = "c") 
    && ((length t1_2) = 2)  && ((length t1_2s) = 2) 
    && ((head t1_2) = "b")  && ((head t1_2s) = "b") 
    && ((length t1_1) = 1)  && ((length t1_1s) = 1) 
    && ((head t1_1) = "a")  && ((head t1_1s) = "a") ) |> should equal true

[<Test>]
let ``head, tail, and length work test 9``() =
    let t1 = tail lena
    let t1s = tail lenasnoc
    let t1_8 = tail t1
    let t1_8s = tail t1s
    let t1_7 = tail t1_8
    let t1_7s = tail t1_8s
    let t1_6 = tail t1_7
    let t1_6s = tail t1_7s
    let t1_5 = tail t1_6
    let t1_5s = tail t1_6s
    let t1_4 = tail t1_5
    let t1_4s = tail t1_5s
    let t1_3 = tail t1_4
    let t1_3s = tail t1_4s
    let t1_2 = tail t1_3
    let t1_2s = tail t1_3s
    let t1_1 = tail t1_2
    let t1_1s = tail t1_2s
    
    (((length t1) = 9) && ((length t1s) = 9) && ((head t1) = "i")  && ((head t1s) = "i") 
    && ((length t1_8) = 8) && ((length t1_8s) = 8) && ((head t1_8) = "h") && ((head t1_8s) = "h") 
    && ((length t1_7) = 7) && ((length t1_7s) = 7) && ((head t1_7) = "g") && ((head t1_7s) = "g") 
    && ((length t1_6) = 6) && ((length t1_6s) = 6) && ((head t1_6) = "f") && ((head t1_6s) = "f") 
    && ((length t1_5) = 5) && ((length t1_5s) = 5) && ((head t1_5) = "e") && ((head t1_5s) = "e") 
    && ((length t1_4) = 4) && ((length t1_4s) = 4) && ((head t1_4) = "d") && ((head t1_4s) = "d") 
    && ((length t1_3) = 3) && ((length t1_3s) = 3) && ((head t1_3) = "c") && ((head t1_3s) = "c") 
    && ((length t1_2) = 2) && ((length t1_2s) = 2) && ((head t1_2) = "b") && ((head t1_2s) = "b") 
    && ((length t1_1) = 1) && ((length t1_1s) = 1) && ((head t1_1) = "a") && ((head t1_1s) = "a")) |> should equal true

[<Test>]
//the previous series thoroughly tested construction by snoc, so we'll leave those out
let ``last, init, and length work test 1``() =  
    let t1 = init len2
    
    (((length t1) = 1) && ((last t1) = "b")) |> should equal true

[<Test>]
let ``last, init, and length work test 2``() =
    let t1 = init len3
    let t1_1 = init t1
    
    (((length t1) = 2) && ((last t1) = "b") && ((length t1_1) = 1)  && ((last t1_1) = "c") ) |> should equal true

[<Test>]
let ``last, init, and length work test 3``() =
    let t1 = init len4
    let t1_1 = init t1
    let t1_2 = init t1_1
    
    (((length t1) = 3) && ((last t1) = "b")
    && ((length t1_1) = 2)  && ((last t1_1) = "c") 
    && ((length t1_2) = 1)  && ((last t1_2) = "d") ) |> should equal true

[<Test>]
let ``last, init, and length work test 4``() =
    let t1 = init len5
    let t1_1 = init t1
    let t1_2 = init t1_1
    let t1_3 = init t1_2
    
    (((length t1) = 4) && ((last t1) = "b")
    && ((length t1_1) = 3)  && ((last t1_1) = "c") 
    && ((length t1_2) = 2)  && ((last t1_2) = "d") 
    && ((length t1_3) = 1)  && ((last t1_3) = "e") ) |> should equal true

[<Test>]
let ``last, init, and length work test 5``() =
    let t1 = init len6
    let t1_1 = init t1
    let t1_2 = init t1_1
    let t1_3 = init t1_2
    let t1_4 = init t1_3
    
    (((length t1) = 5) && ((last t1) = "b")
    && ((length t1_1) = 4)  && ((last t1_1) = "c") 
    && ((length t1_2) = 3)  && ((last t1_2) = "d") 
    && ((length t1_3) = 2)  && ((last t1_3) = "e") 
    && ((length t1_4) = 1)  && ((last t1_4) = "f") ) |> should equal true

[<Test>]
let ``last, init, and length work test 6``() =
    let t1 = init len7
    let t1_1 = init t1
    let t1_2 = init t1_1
    let t1_3 = init t1_2
    let t1_4 = init t1_3
    let t1_5 = init t1_4
    
    (((length t1) = 6) && ((last t1) = "b")
    && ((length t1_1) = 5)  && ((last t1_1) = "c") 
    && ((length t1_2) = 4)  && ((last t1_2) = "d") 
    && ((length t1_3) = 3)  && ((last t1_3) = "e") 
    && ((length t1_4) = 2)  && ((last t1_4) = "f") 
    && ((length t1_5) = 1)  && ((last t1_5) = "g") ) |> should equal true

[<Test>]
let ``last, init, and length work test 7``() =
    let t1 = init len8
    let t1_1 = init t1
    let t1_2 = init t1_1
    let t1_3 = init t1_2
    let t1_4 = init t1_3
    let t1_5 = init t1_4
    let t1_6 = init t1_5
    
    (((length t1) = 7) && ((last t1) = "b") 
    && ((length t1_1) = 6)  && ((last t1_1) = "c") 
    && ((length t1_2) = 5)  && ((last t1_2) = "d") 
    && ((length t1_3) = 4)  && ((last t1_3) = "e") 
    && ((length t1_4) = 3)  && ((last t1_4) = "f") 
    && ((length t1_5) = 2)  && ((last t1_5) = "g") 
    && ((length t1_6) = 1)  && ((last t1_6) = "h") ) |> should equal true

[<Test>]
let ``last, init, and length work test 8``() =
    let t1 = init len9
    let t1_1 = init t1
    let t1_2 = init t1_1
    let t1_3 = init t1_2
    let t1_4 = init t1_3
    let t1_5 = init t1_4
    let t1_6 = init t1_5
    let t1_7 = init t1_6
    
    (((length t1) = 8) && ((last t1) = "b")
    && ((length t1_1) = 7)  && ((last t1_1) = "c") 
    && ((length t1_2) = 6)  && ((last t1_2) = "d") 
    && ((length t1_3) = 5)  && ((last t1_3) = "e") 
    && ((length t1_4) = 4)  && ((last t1_4) = "f") 
    && ((length t1_5) = 3)  && ((last t1_5) = "g") 
    && ((length t1_6) = 2)  && ((last t1_6) = "h") 
    && ((length t1_7) = 1)  && ((last t1_7) = "i") ) |> should equal true

[<Test>]
let ``last, init, and length work test 9``() =
    let t1 = init lena
    let t1_1 = init t1
    let t1_2 = init t1_1
    let t1_3 = init t1_2
    let t1_4 = init t1_3
    let t1_5 = init t1_4
    let t1_6 = init t1_5
    let t1_7 = init t1_6
    let t1_8 = init t1_7
    
    (((length t1) = 9) && ((last t1) = "b")
    && ((length t1_1) = 8)  && ((last t1_1) = "c") 
    && ((length t1_2) = 7)  && ((last t1_2) = "d") 
    && ((length t1_3) = 6)  && ((last t1_3) = "e") 
    && ((length t1_4) = 5)  && ((last t1_4) = "f") 
    && ((length t1_5) = 4)  && ((last t1_5) = "g") 
    && ((length t1_6) = 3)  && ((last t1_6) = "h") 
    && ((length t1_7) = 2)  && ((last t1_7) = "i") 
    && ((length t1_8) = 1)  && ((last t1_8) = "j") ) |> should equal true

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
    ((lena :> IDeque<'a>).Cons "zz") :?> Deque<'a> |> head |> should equal "zz"

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