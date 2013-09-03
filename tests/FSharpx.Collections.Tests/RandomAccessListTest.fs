module FSharpx.Collections.Tests.RandomAccessListTest

open System
open FSharpx.Collections
open FSharpx.Collections.RandomAccessList
open FSharpx.Tests.Properties
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

//there's a crap-load to test here :)
//vector blocksizej of 32, need to generate lists up to 100

let emptyRandomAccessList = RandomAccessList.empty

let consThruList l q  =
    let rec loop (q' : 'a RandomAccessList) (l' : 'a list) = 
        match l' with
        | hd :: [] -> q'.Cons hd
        | hd :: tl -> loop (q'.Cons hd) tl
        | [] -> q'
        
    loop q l

//RandomAccessList
(*
non-IRandomAccessList generators from random ofList
*)
let RandomAccessListOfListGen =
    gen {   let! n = Gen.length2thru100
            let! x = Gen.listInt n
            return ( (RandomAccessList.ofSeq x), x) }

(*
IRandomAccessList generators from random ofSeq and/or conj elements from random list 
*)
let RandomAccessListIntGen =
    gen {   let! n = Gen.length1thru100
            let! n2 = Gen.length2thru100
            let! x =  Gen.listInt n
            let! y =  Gen.listInt n2
            return ( (RandomAccessList.ofSeq x |> consThruList y), ((List.rev y) @ x) ) }

let RandomAccessListIntOfSeqGen =
    gen {   let! n = Gen.length1thru100
            let! x = Gen.listInt n
            return ( (RandomAccessList.ofSeq x), x) }

let RandomAccessListIntConsGen =
    gen {   let! n = Gen.length1thru100
            let! x = Gen.listInt n
            return ( (RandomAccessList.empty |> consThruList x),  List.rev x) }

let RandomAccessListObjGen =
    gen {   let! n = Gen.length2thru100
            let! n2 = Gen.length1thru100
            let! x =  Gen.listObj n
            let! y =  Gen.listObj n2
            return ( (RandomAccessList.ofSeq x |> consThruList y), ((List.rev y) @ x) ) }

let RandomAccessListStringGen =
    gen {   let! n = Gen.length1thru100
            let! n2 = Gen.length2thru100
            let! x =  Gen.listString n
            let! y =  Gen.listString n2  
            return ( (RandomAccessList.ofSeq x |> consThruList y), ((List.rev y) @ x) ) }

// NUnit TestCaseSource does not understand array of tuples at runtime
let intGens start =
    let v = Array.create 3 (box (RandomAccessListIntGen, "RandomAccessList"))
    v.[1] <- box ((RandomAccessListIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "RandomAccessList OfSeq")
    v.[2] <- box ((RandomAccessListIntConsGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "RandomAccessList conjRandomAccessList") 
    v

let intGensStart1 =
    intGens 1  //this will accept all

let intGensStart2 =
    intGens 2 // this will accept 11 out of 12

[<Test>]
let ``fail if there is no head in the RandomAccessList``() =
    (fun () -> emptyRandomAccessList |> head |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``fail if there is no tail in the RandomAccessList``() =
    (fun () -> emptyRandomAccessList |> tail |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``fold matches build list rev``() =

    fsCheck "RandomAccessList" (Prop.forAll (Arb.fromGen RandomAccessListIntGen) 
        (fun ((q :RandomAccessList<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))
              
    fsCheck "RandomAccessList OfSeq" (Prop.forAll (Arb.fromGen RandomAccessListIntOfSeqGen) 
        (fun ((q :RandomAccessList<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

    fsCheck "RandomAccessList Cons" (Prop.forAll (Arb.fromGen RandomAccessListIntConsGen) 
         (fun ((q :RandomAccessList<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

[<Test>]
let ``foldBack matches build list``() =

    fsCheck "RandomAccessList" (Prop.forAll (Arb.fromGen RandomAccessListIntGen) 
        (fun ((q :RandomAccessList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))
              
    fsCheck "RandomAccessList OfSeq" (Prop.forAll (Arb.fromGen RandomAccessListIntOfSeqGen) 
        (fun ((q :RandomAccessList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

    fsCheck "RandomAccessList Conj" (Prop.forAll (Arb.fromGen RandomAccessListIntConsGen) 
         (fun ((q :RandomAccessList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

[<Test>]
let ``foldBack matches build list 2``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"]
    let lq = foldBack (fun (elem : string) (l' : string list) -> elem::l') q []
    lq |> should equal (List.ofSeq q)

[<Test>]
let ``fold matches build list rev 2``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"]
    let lq = fold (fun (l' : string list) (elem : string) -> elem::l') [] q
    lq |> should equal (List.rev (List.ofSeq q))

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``get head from RandomAccessList``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : RandomAccessList<int>, l) -> (head q) = (List.nth l 0) ))

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``get head from RandomAccessList safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : RandomAccessList<int>, l) -> (tryHead q).Value = (List.nth l 0) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get tail from RandomAccessList``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((q : RandomAccessList<int>), l) -> q.Tail.Head = (List.nth l 1) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get tail from RandomAccessList safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : RandomAccessList<int>, l) -> q.TryTail.Value.Head = (List.nth l 1) ))

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``int RandomAccessList builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : RandomAccessList<int>, l) -> q |> Seq.toList = l ))

[<Test>]
let ``obj RandomAccessList builds and serializes``() =
    fsCheck "obj RandomAccessList" (Prop.forAll (Arb.fromGen RandomAccessListObjGen) (fun (q : RandomAccessList<obj>, l) -> q |> Seq.toList = l ))

[<Test>]
let ``string RandomAccessList builds and serializes``() =
    fsCheck "string RandomAccessList" (Prop.forAll (Arb.fromGen RandomAccessListStringGen) (fun (q : RandomAccessList<string>, l) -> q |> Seq.toList = l ))

[<Test>]
let ``TryUncons wind-down to None``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"] 

    let rec loop (q' : RandomAccessList<string>) = 
        match (q'.TryUncons) with
        | Some(hd, tl) ->  loop tl
        | None -> ()

    loop q

    true |> should equal true

[<Test>]
let ``Uncons wind-down to None``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"] 

    let rec loop (q' : RandomAccessList<string>) = 
        match (q'.Uncons) with
        | hd, tl when tl.IsEmpty ->  ()
        | hd, tl ->  loop tl

    loop q

    true |> should equal true

[<Test>]
let ``empty list should be empty``() =
    empty |> isEmpty |> should equal true

[<Test>]
let ``cons works``() =
    empty|> cons 1 |> cons 2 |> isEmpty |> should equal false

[<Test>]
let ``uncons 1 element``() =
    let x, _ = empty |> cons 1 |>  uncons
    (x = 1) |> should equal true

[<Test>]
let ``uncons 2 elements``() =
    let x, _ = empty |> cons 1 |> cons 2 |> uncons 
    (x = 2) |> should equal true

[<Test>]
let ``uncons 3 elements``() =
    let x, _ = empty |> cons 1 |> cons 2 |> cons 3 |> uncons 
    (x = 3) |> should equal true

[<Test>]
let ``tryUncons 1 element``() =
    let x = empty |> cons 1 |> tryUncons
    (fst(x.Value) = 1) |> should equal true

[<Test>]
let ``tryUncons 2 elements``() =
    let x = empty |> cons 1 |> cons 2 |> tryUncons
    (fst(x.Value) = 2) |> should equal true

[<Test>]
let ``tryUncons 3 elements``() =
    let x = empty |> cons 1 |> cons 2 |> cons 3 |> tryUncons 
    (fst(x.Value) = 3) |> should equal true

[<Test>]
let ``tryUncons empty``() =
    empty |> tryUncons |> should equal None
    
[<Test>]
let ``head should return``() =
    let x = empty |> cons 1 |> cons 2 |> head 
    x |> should equal 2

[<Test>]
let ``tryHead should return``() =
    let x = empty |> cons 1 |> cons 2 |> tryHead 
    x.Value |> should equal 2

[<Test>]
let ``tryHead on empty should return None``() =
    empty |> tryHead |> should equal None

[<Test>]
let ``tryTail on empty should return None``() =
    empty |> tryTail |> should equal None

[<Test>]
let ``tryTail on len 1 should return Some empty``() =
    let x = (empty |> cons 1 |> tryTail).Value
    x |> isEmpty |> should equal true

[<Test>]
let ``tail on len 2 should return``() =
    empty |> cons 1 |>  cons 2 |> tail |> head |> should equal 1

[<Test>]
let ``tryTail on len 2 should return``() =
    let a = empty |> cons 1 |>  cons 2 |> tryTail 
    ((head a.Value) = 1) |> should equal true

[<Test>]
let ``randomAccessList of randomAccessLists constructed by consing tail``() =

    let windowFun windowLength = 
        fun (v : RandomAccessList<RandomAccessList<int>>) t ->
        if v.Head.Length = windowLength then RandomAccessList.cons (RandomAccessList.empty.Cons(t)) v
        else RandomAccessList.tail v |> RandomAccessList.cons (RandomAccessList.cons t (RandomAccessList.head v))

    let windowed = 
        seq{1..100}
        |> Seq.fold (windowFun 5) (RandomAccessList.empty.Cons RandomAccessList.empty<int>)

    windowed.Length |> should equal 20
    windowed.[2].Length |> should equal 5

[<Test>]
let ``nth length 1``() =
    let x = empty |> cons "a" 
//    let x = empty |> cons "a" 
    let x' = x |> nth 0 
    x' |> should equal "a"

[<Test>]
let ``rev empty``() =
    isEmpty (empty |> rev) |> should equal true
    
[<Test>]
let ``rev elements length 5``() =
    let a = ofSeq ["a";"b";"c";"d";"e"]

    let b = rev a

    let c = List.ofSeq b
    a.Head |> should equal "a"
    b.Head |> should equal "e"
    c.Head |> should equal "e"
    b |> List.ofSeq |> should equal (["a";"b";"c";"d";"e"] |> List.rev)

[<Test>]
let ``rev elements length 15``() =
    let a = ofSeq ["a";"b";"c";"d";"e"; "f"; "g"; "h"; "i"; "j"; "l"; "m"; "n"; "o"; "p"]
    let b = rev a
    b |> List.ofSeq |> should equal (["a";"b";"c";"d";"e"; "f"; "g"; "h"; "i"; "j"; "l"; "m"; "n"; "o"; "p"] |> List.rev)

[<Test>]
let ``rev 300``() =
    let x = ofSeq [1..300]
    x.Rev() |> List.ofSeq  |> should equal (List.rev [1..300])

[<Test>]
let ``rev matches build list rev``() =

    fsCheck "RandomAccessList" (Prop.forAll (Arb.fromGen RandomAccessListIntGen) 
        (fun ((q :RandomAccessList<int>), (l : int list)) -> q |> rev |> List.ofSeq = (List.rev l) ))
              
    fsCheck "RandomAccessList OfSeq" (Prop.forAll (Arb.fromGen RandomAccessListIntOfSeqGen) 
        (fun ((q :RandomAccessList<int>), (l : int list)) -> q |> rev |> List.ofSeq = (List.rev l) ))

    fsCheck "RandomAccessList Cons" (Prop.forAll (Arb.fromGen RandomAccessListIntConsGen) 
         (fun ((q :RandomAccessList<int>), (l : int list)) -> q |> rev |> List.ofSeq = (List.rev l) ))



[<Test>]
let ``nth length 2``() =
    (((empty |> cons "a" |> cons "b" |> nth 0) = "b") && ((empty |> cons "a" |> cons "b" |> nth 1) = "a")) |> should equal true

[<Test>]
let ``nth length 3``() =
    let len3 = empty |> cons "a" |> cons "b" |> cons "c"
    (((len3 |> nth 0) = "c") 
    && ((len3 |> nth 1) = "b") 
    && ((len3 |> nth 2) = "a")) |> should equal true

[<Test>]
let ``nth length 4``() =
    let len4 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d"
    (((len4 |> nth 0) = "d") && ((len4 |> nth 1) = "c") && ((len4 |> nth 2) = "b") && ((len4 |> nth 3) = "a")) 
    |> should equal true

[<Test>]
let ``nth length 5``() =
    let len5 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e"
    (((len5 |> nth 0) = "e") && ((len5 |> nth 1) = "d") && ((len5 |> nth 2) = "c") && ((len5 |> nth 3) = "b") 
    && ((len5 |> nth 4) = "a")) |> should equal true

[<Test>]
let ``nth length 6``() =
    let len6 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f"
    (((len6 |> nth 0) = "f") && ((len6 |> nth 1) = "e") && ((len6 |> nth 2) = "d") && ((len6 |> nth 3) = "c") 
    && ((len6 |> nth 4) = "b") && ((len6 |> nth 5) = "a")) |> should equal true

[<Test>]
let ``nth length 7``() =
    let len7 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g"
    (((len7 |> nth 0) = "g") && ((len7 |> nth 1) = "f") && ((len7 |> nth 2) = "e") && ((len7 |> nth 3) = "d") 
    && ((len7 |> nth 4) = "c") && ((len7 |> nth 5) = "b") && ((len7 |> nth 6) = "a")) |> should equal true

[<Test>]
let ``nth length 8``() =
    let len8 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h"
    (((len8 |> nth 0) = "h") && ((len8 |> nth 1) = "g") && ((len8 |> nth 2) = "f") && ((len8 |> nth 3) = "e") 
    && ((len8 |> nth 4) = "d") && ((len8 |> nth 5) = "c") && ((len8 |> nth 6) = "b") && ((len8 |> nth 7) = "a")) 
    |> should equal true

[<Test>]
let ``nth length 9``() =
    let len9 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i"
    (((len9 |> nth 0) = "i") && ((len9 |> nth 1) = "h") && ((len9 |> nth 2) = "g") && ((len9 |> nth 3) = "f") 
    && ((len9 |> nth 4) = "e") && ((len9 |> nth 5) = "d") && ((len9 |> nth 6) = "c") && ((len9 |> nth 7) = "b")
    && ((len9 |> nth 8) = "a")) |> should equal true

[<Test>]
let ``nth length 10``() =
    let lena = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i" |> cons "j"
    (((lena |> nth 0) = "j") && ((lena |> nth 1) = "i") && ((lena |> nth 2) = "h") && ((lena |> nth 3) = "g") 
    && ((lena |> nth 4) = "f") && ((lena |> nth 5) = "e") && ((lena |> nth 6) = "d") && ((lena |> nth 7) = "c")
    && ((lena |> nth 8) = "b") && ((lena |> nth 9) = "a")) |> should equal true

[<Test>]
let ``tryNth length 1``() =
    let a = empty |> cons "a" |> tryNth 0 
    (a.Value = "a") |> should equal true

[<Test>]
let ``tryNth length 2``() =
    let len2 = empty |> cons "a" |> cons "b"
    let b = len2 |> tryNth 0
    let a = len2 |> tryNth 1
    ((b.Value = "b") && (a.Value = "a")) |> should equal true

[<Test>]
let ``tryNth length 3``() =
    let len3 = empty |> cons "a" |> cons "b" |> cons "c"
    let c = len3 |> tryNth 0
    let b = len3 |> tryNth 1
    let a = len3 |> tryNth 2
    ((c.Value = "c") && (b.Value = "b") && (a.Value = "a")) |> should equal true

[<Test>]
let ``tryNth length 4``() =
    let len4 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d"
    let d = len4 |> tryNth 0
    let c = len4 |> tryNth 1
    let b = len4 |> tryNth 2
    let a = len4 |> tryNth 3
    ((d.Value = "d") && (c.Value = "c") && (b.Value = "b") && (a.Value = "a")) |> should equal true 

[<Test>]
let ``tryNth length 5``() =
    let len5 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e"
    let e = len5 |> tryNth 0
    let d = len5 |> tryNth 1
    let c = len5 |> tryNth 2
    let b = len5 |> tryNth 3
    let a = len5 |> tryNth 4
    ((e.Value = "e") && (d.Value = "d") && (c.Value = "c") && (b.Value = "b") && (a.Value = "a")) |> should equal true

[<Test>]
let ``tryNth length 6``() =
    let len6 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f"
    let f = len6 |> tryNth 0
    let e = len6 |> tryNth 1
    let d = len6 |> tryNth 2
    let c = len6 |> tryNth 3
    let b = len6 |> tryNth 4
    let a = len6 |> tryNth 5
    ((f.Value = "f") && (e.Value = "e") && (d.Value = "d") && (c.Value = "c") && (b.Value = "b") && (a.Value = "a")) 
    |> should equal true

[<Test>]
let ``tryNth length 7``() =
    let len7 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g"
    let g = len7 |> tryNth 0
    let f = len7 |> tryNth 1
    let e = len7 |> tryNth 2
    let d = len7 |> tryNth 3
    let c = len7 |> tryNth 4
    let b = len7 |> tryNth 5
    let a = len7 |> tryNth 6
    ((g.Value = "g") && (f.Value = "f") && (e.Value = "e") && (d.Value = "d") && (c.Value = "c") && (b.Value = "b") 
    && (a.Value = "a")) |> should equal true

[<Test>]
let ``tryNth length 8``() =
    let len8 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h"
    let h = len8 |> tryNth 0
    let g = len8 |> tryNth 1
    let f = len8 |> tryNth 2
    let e = len8 |> tryNth 3
    let d = len8 |> tryNth 4
    let c = len8 |> tryNth 5
    let b = len8 |> tryNth 6
    let a = len8 |> tryNth 7
    ((h.Value = "h") && (g.Value = "g") && (f.Value = "f") && (e.Value = "e") && (d.Value = "d") && (c.Value = "c")  
    && (b.Value = "b")&& (a.Value = "a")) |> should equal true

[<Test>]
let ``tryNth length 9``() =
    let len9 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i"
    let i = len9 |> tryNth 0
    let h = len9 |> tryNth 1
    let g = len9 |> tryNth 2
    let f = len9 |> tryNth 3
    let e = len9 |> tryNth 4
    let d = len9 |> tryNth 5
    let c = len9 |> tryNth 6
    let b = len9 |> tryNth 7
    let a = len9 |> tryNth 8
    ((i.Value = "i") && (h.Value = "h") && (g.Value = "g") && (f.Value = "f") && (e.Value = "e") && (d.Value = "d") 
    && (c.Value = "c") && (b.Value = "b")&& (a.Value = "a")) |> should equal true

[<Test>]
let ``tryNth length 10``() =
    let lena = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i" |> cons "j"
    let j = lena |> tryNth 0
    let i = lena |> tryNth 1
    let h = lena |> tryNth 2
    let g = lena |> tryNth 3
    let f = lena |> tryNth 4
    let e = lena |> tryNth 5
    let d = lena |> tryNth 6
    let c = lena |> tryNth 7
    let b = lena |> tryNth 8
    let a = lena |> tryNth 9
    ((j.Value = "j") && (i.Value = "i") && (h.Value = "h") && (g.Value = "g") && (f.Value = "f") && (e.Value = "e") 
    && (d.Value = "d") && (c.Value = "c") && (b.Value = "b")&& (a.Value = "a")) |> should equal true

[<Test>]
let ``tryNth not found``() =
    let lena = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i" |> cons "j"
    lena |> tryNth 10 |> should equal None

[<Test>]
let ``update length 1``() =
    empty |> cons "a" |> update 0 "aa"|> nth 0 |> should equal "aa"

[<Test>]
let ``update length 2``() =
    let len2 = empty |> cons "a" |> cons "b"
    (((len2 |> update 0 "bb"|> nth 0) = "bb") && ((len2 |> update 1 "aa"|> nth 1) = "aa")) |> should equal true

[<Test>]
let ``update length 3``() =
    let len3 = empty |> cons "a" |> cons "b" |> cons "c"
    (((len3 |> update 0 "cc"|> nth 0) = "cc") && ((len3 |> update 1 "bb"|> nth 1) = "bb") 
    && ((len3 |> update 2 "aa"|> nth 2) = "aa")) |> should equal true

[<Test>]
let ``update length 4``() =
    let len4 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d"
    (((len4 |> update 0 "dd"|> nth 0) = "dd") && ((len4 |> update 1 "cc"|> nth 1) = "cc") 
    && ((len4 |> update 2 "bb"|> nth 2) = "bb") && ((len4 |> update 3 "aa"|> nth 3) = "aa")) 
    |> should equal true

[<Test>]
let ``update length 5``() =
    let len5 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e"
    (((len5 |> update 0 "ee"|> nth 0) = "ee") && ((len5 |> update 1 "dd"|> nth 1) = "dd") 
    && ((len5 |> update 2 "cc"|> nth 2) = "cc") && ((len5 |> update 3 "bb"|> nth 3) = "bb") 
    && ((len5 |> update 4 "aa"|> nth 4) = "aa")) |> should equal true

[<Test>]
let ``update length 6``() =
    let len6 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f"
    (((len6 |> update 0 "ff"|> nth 0) = "ff") && ((len6 |> update 1 "ee"|> nth 1) = "ee") 
    && ((len6 |> update 2 "dd"|> nth 2) = "dd") && ((len6 |> update 3 "cc"|> nth 3) = "cc") 
    && ((len6 |> update 4 "bb"|> nth 4) = "bb") && ((len6 |> update 5 "aa"|> nth 5) = "aa")) |> should equal true

[<Test>]
let ``update length 7``() =
    let len7 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g"
    (((len7 |> update 0 "gg"|> nth 0) = "gg") && ((len7 |> update 1 "ff"|> nth 1) = "ff") 
    && ((len7 |> update 2 "ee"|> nth 2) = "ee") && ((len7 |> update 3 "dd"|> nth 3) = "dd") 
    && ((len7 |> update 4 "cc"|> nth 4) = "cc") && ((len7 |> update 5 "bb"|> nth 5) = "bb") 
    && ((len7 |> update 6 "aa"|> nth 6) = "aa")) |> should equal true

[<Test>]
let ``update length 8``() =
    let len8 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h"
    (((len8 |> update 0 "hh"|> nth 0) = "hh") && ((len8 |> update 1 "gg"|> nth 1) = "gg") 
    && ((len8 |> update 2 "ff"|> nth 2) = "ff") && ((len8 |> update 3 "ee"|> nth 3) = "ee") 
    && ((len8 |> update 4 "dd"|> nth 4) = "dd") && ((len8 |> update 5 "cc"|> nth 5) = "cc") 
    && ((len8 |> update 6 "bb"|> nth 6) = "bb") && ((len8 |> update 7 "aa"|> nth 7) = "aa")) 
    |> should equal true

[<Test>]
let ``update length 9``() =
    let len9 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i"
    (((len9 |> update 0 "ii"|> nth 0) = "ii") && ((len9 |> update 1 "hh"|> nth 1) = "hh") 
    && ((len9 |> update 2 "gg"|> nth 2) = "gg") && ((len9 |> update 3 "ff"|> nth 3) = "ff") 
    && ((len9 |> update 4 "ee"|> nth 4) = "ee") && ((len9 |> update 5 "dd"|> nth 5) = "dd") 
    && ((len9 |> update 6 "cc"|> nth 6) = "cc") && ((len9 |> update 7 "bb"|> nth 7) = "bb")
    && ((len9 |> update 8 "aa"|> nth 8) = "aa")) |> should equal true

[<Test>]
let ``update length 10``() =
    let lena = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i" |> cons "j"
    (((lena |> update 0 "jj"|> nth 0) = "jj") && ((lena |> update 1 "ii"|> nth 1) = "ii") 
    && ((lena |> update 2 "hh"|> nth 2) = "hh") && ((lena |> update 3 "gg"|> nth 3) = "gg") 
    && ((lena |> update 4 "ff"|> nth 4) = "ff") && ((lena |> update 5 "ee"|> nth 5) = "ee") 
    && ((lena |> update 6 "dd"|> nth 6) = "dd") && ((lena |> update 7 "cc"|> nth 7) = "cc")
    && ((lena |> update 8 "bb"|> nth 8) = "bb") && ((lena |> update 9 "aa"|> nth 9) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 1``() =
    let a = empty |> cons "a" |> tryUpdate 0 "aa"
    ((a.Value |> nth 0) = "aa") |> should equal true

[<Test>]
let ``tryUpdate length 2``() =
    let len2 = empty |> cons "a" |> cons "b"
    let b = len2 |> tryUpdate 0 "bb"
    let a = len2 |> tryUpdate 1 "aa"
    (((b.Value |> nth 0) = "bb") && ((a.Value |> nth 1) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 3``() =
    let len3 = empty |> cons "a" |> cons "b" |> cons "c"
    let c = len3 |> tryUpdate 0 "cc"
    let b = len3 |> tryUpdate 1 "bb"
    let a = len3 |> tryUpdate 2 "aa"
    (((c.Value |> nth 0) = "cc") && ((b.Value |> nth 1) = "bb") && ((a.Value |> nth 2) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 4``() =
    let len4 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d"
    let d = len4 |> tryUpdate 0 "dd"
    let c = len4 |> tryUpdate 1 "cc"
    let b = len4 |> tryUpdate 2 "bb"
    let a = len4 |> tryUpdate 3 "aa"
    (((d.Value |> nth 0) = "dd") && ((c.Value |> nth 1) = "cc") && ((b.Value |> nth 2) = "bb") 
    && ((a.Value |> nth 3) = "aa")) |> should equal true 

[<Test>]
let ``tryUpdate length 5``() =
    let len5 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e"
    let e = len5 |> tryUpdate 0 "ee"
    let d = len5 |> tryUpdate 1 "dd"
    let c = len5 |> tryUpdate 2 "cc"
    let b = len5 |> tryUpdate 3 "bb"
    let a = len5 |> tryUpdate 4 "aa"
    (((e.Value |> nth 0) = "ee") && ((d.Value |> nth 1) = "dd") && ((c.Value |> nth 2) = "cc") 
    && ((b.Value |> nth 3) = "bb") && ((a.Value |> nth 4) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 6``() =
    let len6 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f"
    let f = len6 |> tryUpdate 0 "ff"
    let e = len6 |> tryUpdate 1 "ee"
    let d = len6 |> tryUpdate 2 "dd"
    let c = len6 |> tryUpdate 3 "cc"
    let b = len6 |> tryUpdate 4 "bb"
    let a = len6 |> tryUpdate 5 "aa"
    (((f.Value |> nth 0) = "ff") && ((e.Value |> nth 1) = "ee") && ((d.Value |> nth 2) = "dd") 
    && ((c.Value |> nth 3) = "cc") && ((b.Value |> nth 4) = "bb") && ((a.Value |> nth 5) = "aa")) 
    |> should equal true

[<Test>]
let ``tryUpdate length 7``() =
    let len7 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g"
    let g = len7 |> tryUpdate 0 "gg"
    let f = len7 |> tryUpdate 1 "ff"
    let e = len7 |> tryUpdate 2 "ee"
    let d = len7 |> tryUpdate 3 "dd"
    let c = len7 |> tryUpdate 4 "cc"
    let b = len7 |> tryUpdate 5 "bb"
    let a = len7 |> tryUpdate 6 "aa"
    (((g.Value |> nth 0) = "gg") && ((f.Value |> nth 1) = "ff") && ((e.Value |> nth 2) = "ee") 
    && ((d.Value |> nth 3) = "dd") && ((c.Value |> nth 4) = "cc") && ((b.Value |> nth 5) = "bb") 
    && ((a.Value |> nth 6) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 8``() =
    let len8 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h"
    let h = len8 |> tryUpdate 0 "hh"
    let g = len8 |> tryUpdate 1 "gg"
    let f = len8 |> tryUpdate 2 "ff"
    let e = len8 |> tryUpdate 3 "ee"
    let d = len8 |> tryUpdate 4 "dd"
    let c = len8 |> tryUpdate 5 "cc"
    let b = len8 |> tryUpdate 6 "bb"
    let a = len8 |> tryUpdate 7 "aa"
    (((h.Value |> nth 0) = "hh") && ((g.Value |> nth 1) = "gg") && ((f.Value |> nth 2) = "ff") 
    && ((e.Value |> nth 3) = "ee") && ((d.Value |> nth 4) = "dd") && ((c.Value |> nth 5) = "cc")  
    && ((b.Value |> nth 6) = "bb")&& ((a.Value |> nth 7) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 9``() =
    let len9 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i"
    let i = len9 |> tryUpdate 0 "ii"
    let h = len9 |> tryUpdate 1 "hh"
    let g = len9 |> tryUpdate 2 "gg"
    let f = len9 |> tryUpdate 3 "ff"
    let e = len9 |> tryUpdate 4 "ee"
    let d = len9 |> tryUpdate 5 "dd"
    let c = len9 |> tryUpdate 6 "cc"
    let b = len9 |> tryUpdate 7 "bb"
    let a = len9 |> tryUpdate 8 "aa"
    (((i.Value |> nth 0) = "ii") && ((h.Value |> nth 1) = "hh") && ((g.Value |> nth 2) = "gg") 
    && ((f.Value |> nth 3) = "ff") && ((e.Value |> nth 4) = "ee") && ((d.Value |> nth 5) = "dd") 
    && ((c.Value |> nth 6) = "cc") && ((b.Value |> nth 7) = "bb")&& ((a.Value |> nth 8) = "aa")) |> should equal true

[<Test>]
let ``tryUpdate length 10``() =
    let lena = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i" |> cons "j"
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
    (((j.Value |> nth 0) = "jj") && ((i.Value |> nth 1) = "ii") && ((h.Value |> nth 2) = "hh") 
    && ((g.Value |> nth 3) = "gg") && ((f.Value |> nth 4) = "ff") && ((e.Value |> nth 5) = "ee") 
    && ((d.Value |> nth 6) = "dd") && ((c.Value |> nth 7) = "cc") && ((b.Value |> nth 8) = "bb")
    && ((a.Value |> nth 9) = "aa")) |> should equal true


[<Test>]
let ``tryupdate of long RAL``() =
    let v = ofSeq [1..100]

    v |> update 99 5 |> nth 99 |> should equal 5

[<Test>]
let ``length of empty is 0``() =
    empty |> length |> should equal 0

[<Test>]
let ``length of 1 - 10 good``() =
    let len1 = empty |> cons "a"
    let len2 = empty |> cons "a" |> cons "b"
    let len3 = empty |> cons "a" |> cons "b" |> cons "c"
    let len4 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d"
    let len5 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e"
    let len6 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f"
    let len7 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g"
    let len8 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h"
    let len9 = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i"
    let lena = empty |> cons "a" |> cons "b" |> cons "c" |> cons "d" |> cons "e" |> cons "f" |> cons "g" |> cons "h" |> cons "i" |> cons "j"
    (((length len1) = 1) && ((length len2) = 2) && ((length len3) = 3) && ((length len4) = 4) 
    && ((length len5) = 5) && ((length len6) = 6) && ((length len7) = 7) && ((length len8) = 8) 
    && ((length len9) = 9) && ((length lena) = 10)) |> should equal true

[<Test>]
let ``allow map``() =
    let x = ofSeq [1..300]
    let randomAccessList2 = map (fun x -> x * 2) x
    randomAccessList2 |> Seq.toList |> should equal [for i in 1..300 -> i * 2]

[<Test>]
let ``cons pattern discriminator - randomAccessList``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"]
    
    let h1, t1 = 
        match q with
        | Cons(h, t) -> h, t
        | _ ->  "x", q

    ((h1 = "f") && (t1.Length = 5)) |> should equal true

[<Test>]
let ``structural equality``() =

    let l1 = ofSeq [1..100]
    let l2 = ofSeq [1..100]

    l1 = l2 |> should equal true

    let l3 = l2 |> update 98 7

    l1 = l3 |> should equal false

[<Test>]
let ``ofSeq random access list``() =
    let x = ofSeq ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

    (((x |> nth 0) = "a") && ((x |> nth 1) = "b") && ((x |> nth 2) = "c") && ((x |> nth 3) = "d") 
    && ((x |> nth 4) = "e") && ((x |> nth 5) = "f") && ((x |> nth 6) = "g") && ((x |> nth 7) = "h")
    && ((x |> nth 8) = "i") && ((x |> nth 9) = "j")) |> should equal true

[<Test>]
let ``allow init``() =
    let randomAccessList = init 5 (fun x -> x * 2) 
    let s = Seq.init 5 (fun x -> x * 2)

    s |> Seq.toList |> should equal [0;2;4;6;8]
    randomAccessList |> Seq.toList |> should equal [0;2;4;6;8]

[<Test>]
let ``toSeq to list``() =
    let l = ["f";"e";"d";"c";"b";"a"] 
    let rl = ofSeq l

    rl |> toSeq |> List.ofSeq |> should equal l
