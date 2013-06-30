module FSharpx.Collections.Tests.Deque

open System
open FSharpx.Collections
open FSharpx.Collections.Deque
open FSharpx.Tests.Properties
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

let emptyDeque = Deque.empty

let conjThruList l q  =
    let rec loop (q' : 'a Deque) (l' : 'a list) = 
        match l' with
        | hd :: tl -> loop (q'.Conj hd) tl
        | [] -> q'
        
    loop q l 

//Deque
(*
non-Deque generators from random ofList
*)
let dequeOfListGen =
    gen {   let! n = Gen.length2thru12
            let! x = Gen.listInt n
            return ( (Deque.ofList x), x) }

(*
Deque generators from random ofSeq and/or conj elements from random list 
*)
let dequeIntGen =
    gen {   let! n = Gen.length1thru12
            let! n2 = Gen.length2thru12
            let! x =  Gen.listInt n
            let! y =  Gen.listInt n2
            return ( (Deque.ofSeq x |> conjThruList y), (x @ y) ) }

let dequeIntOfSeqGen =
    gen {   let! n = Gen.length1thru12
            let! x = Gen.listInt n
            return ( (Deque.ofSeq x), x) }

let dequeIntConjGen =
    gen {   let! n = Gen.length1thru12
            let! x = Gen.listInt n
            return ( (Deque.empty |> conjThruList x), x) }

let dequeObjGen =
    gen {   let! n = Gen.length2thru12
            let! n2 = Gen.length1thru12
            let! x =  Gen.listObj n
            let! y =  Gen.listObj n2
            return ( (Deque.ofSeq x |> conjThruList y), (x @ y) ) }

let dequeStringGen =
    gen {   let! n = Gen.length1thru12
            let! n2 = Gen.length2thru12
            let! x =  Gen.listString n
            let! y =  Gen.listString n2  
            return ( (Deque.ofSeq x |> conjThruList y), (x @ y) ) }

// NUnit TestCaseSource does not understand array of tuples at runtime
let intGens start =
    let v = Array.create 3 (box (dequeIntGen, "Deque"))
    v.[1] <- box ((dequeIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "Deque OfSeq")
    v.[2] <- box ((dequeIntConjGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "Deque Enqueue") 
    v

let intGensStart1 =
    intGens 1  //this will accept all

let intGensStart2 =
    intGens 2 // this will accept 11 out of 12

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

let len1conj = empty |> conj "a"
let len2conj = empty |> conj "b" |> conj "a"
let len3conj = empty |> conj "c" |> conj "b" |> conj "a"
let len4conj = empty |> conj "d" |> conj "c" |> conj "b" |> conj "a"
let len5conj = empty |> conj "e" |> conj "d" |> conj "c" |> conj "b" |> conj "a"
let len6conj = empty |> conj "f" |> conj "e" |> conj "d" |> conj "c" |> conj "b" |> conj "a"
let len7conj = empty |> conj "g" |> conj "f" |> conj "e" |> conj "d" |> conj "c" |> conj "b" |> conj "a"
let len8conj = empty |> conj "h" |> conj "g" |> conj "f" |> conj "e" |> conj "d" |> conj "c" |> conj "b" |> conj "a"
let len9conj = empty |> conj "i" |> conj "h" |> conj "g" |> conj "f" |> conj "e" |> conj "d" |> conj "c" |> conj "b" |> conj "a"
let lenaconj = empty |> conj "j" |> conj "i" |> conj "h" |> conj "g" |> conj "f" |> conj "e" |> conj "d" |> conj "c" |> conj "b" |> conj "a"

[<Test>]
let ``empty dqueue should be empty``() =
    empty |> isEmpty |> should equal true

[<Test>]
let ``cons works``() =
    len2 |> isEmpty |> should equal false

[<Test>]
let ``conj works``() =
    len2conj |> isEmpty |> should equal false

[<Test>]
let ``singleton head works``() =
    len1 |> head |> should equal "a"

[<Test>]
let ``singleton last works``() =
    len1 |> last |> should equal "a"

[<Test>]
let ``fold matches build list rev``() =

    fsCheck "Deque" (Prop.forAll (Arb.fromGen dequeIntGen) 
        (fun ((q :Deque<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))
              
    fsCheck "Deque OfSeq" (Prop.forAll (Arb.fromGen dequeIntOfSeqGen) 
        (fun ((q :Deque<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

    fsCheck "Deque Conj" (Prop.forAll (Arb.fromGen dequeIntConjGen) 
         (fun ((q :Deque<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

[<Test>]
let ``foldback matches build list``() =

    fsCheck "Deque" (Prop.forAll (Arb.fromGen dequeIntGen) 
        (fun ((q : Deque<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list)  -> elem::l') q [] = l ))
              
    fsCheck "Deque OfSeq" (Prop.forAll (Arb.fromGen dequeIntOfSeqGen) 
        (fun ((q : Deque<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

    fsCheck "Deque Conj" (Prop.forAll (Arb.fromGen dequeIntConjGen) 
        (fun ((q : Deque<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``int deque builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Deque<int>, l) -> q |> Seq.toList = l ))

[<Test>]
let ``obj deque builds and serializes``() =
    fsCheck "obj Deque" (Prop.forAll (Arb.fromGen dequeObjGen) (fun (q : Deque<obj>, l) -> q |> Seq.toList = l ))

[<Test>]
let ``string deque builds and serializes``() =
    fsCheck "string Deque" (Prop.forAll (Arb.fromGen dequeStringGen) (fun (q : Deque<string>, l) -> q |> Seq.toList = l ))

[<Test>]
let ``reverse . reverse = id``() =
    
    fsCheck "obj Deque" (Prop.forAll (Arb.fromGen dequeObjGen) 
        (fun (q, l) -> q |> rev |> rev |> Seq.toList = (q |> Seq.toList) ))
    
[<Test>]
let ``ofList build and serialize``() =

    fsCheck "Deque" (Prop.forAll (Arb.fromGen dequeOfListGen) 
        (fun ((q : Deque<int>), (l : int list)) -> q |> Seq.toList = l ))

[<Test>]
let ``TryUncons wind-down to None``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"] 

    let rec loop (q' : Deque<string>) = 
        match (q'.TryUncons) with
        | Some(hd, tl) ->  loop tl
        | None -> ()

    loop q

    true |> should equal true

[<Test>]
let ``Uncons wind-down to None``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"] 

    let rec loop (q' : Deque<string>) = 
        match (q'.Uncons) with
        | hd, tl when tl.Length = 0 ->  ()
        | hd, tl ->  loop tl

    loop q

    true |> should equal true

[<Test>]
let ``toSeq works``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"] 
    let l = List.ofSeq q
    let l' = List.ofSeq (toSeq q)
    l |> should equal l'

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``get head from deque``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Deque<int>, l) -> (head q) = (List.nth l 0) ))

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``get head from deque safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Deque<int>, l) -> (tryHead q).Value = (List.nth l 0) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get tail from deque``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((q : Deque<int>), l) -> q.Tail.Head = (List.nth l 1) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get tail from deque safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Deque<int>, l) -> q.TryTail.Value.Head = (List.nth l 1) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get initial from deque``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((q : Deque<int>), l) -> List.ofSeq (initial q) = (List.rev l |> List.tail |> List.rev) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get initial from deque safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Deque<int>, l) -> List.ofSeq q.TryInitial.Value = (List.rev l |> List.tail |> List.rev) ))

[<Test>]
let ``tail of singleton empty``() =
    len1 |> tail |> isEmpty |> should equal true
    len1conj |> tail |> isEmpty |> should equal true

[<Test>]
let ``tail of tail of 2 empty``() =
    len2 |> tail |> tail |> isEmpty |> should equal true
    len2conj |> tail |> tail |> isEmpty |> should equal true

[<Test>]
let ``initial of singleton empty``() =
    len1 |> initial |> isEmpty |> should equal true
    len1conj |> initial |> isEmpty |> should equal true

[<Test>]
let ``head, tail, and length work test 1``() =
    let t1 = tail len2
    let t1s = tail len2conj
    (((length t1) = 1) && ((length t1s) = 1) && ((head t1) = "a") && ((head t1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 2``() =
    let t1 = tail len3
    let t1s = tail len3conj

    let t1_1 = tail t1
    let t1_1s = tail t1s

    (((length t1) = 2) && ((length t1s) = 2) && ((head t1) = "b") && ((head t1s) = "b") && ((length t1_1) = 1) && ((length t1_1s) = 1) 
    && ((head t1_1) = "a") && ((head t1_1s) = "a")) |> should equal true

[<Test>]
let ``head, tail, and length work test 3``() =
    let t1 = tail len4
    let t1s = tail len4conj

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
    let t1s = tail len5conj

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
    let t1s = tail len6conj

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
    let t1s = tail len7conj

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
    let t1s = tail len8conj
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
    let t1s = tail len9conj
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
    let t1s = tail lenaconj
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
//the previous series thoroughly tested construction by conj, so we'll leave those out
let ``last, init, and length work test 1``() =  
    let t1 = initial len2
    
    (((length t1) = 1) && ((last t1) = "b")) |> should equal true

[<Test>]
let ``last, init, and length work test 2``() =
    let t1 = initial len3
    let t1_1 = initial t1
    
    (((length t1) = 2) && ((last t1) = "b") && ((length t1_1) = 1)  && ((last t1_1) = "c") ) |> should equal true

[<Test>]
let ``last, init, and length work test 3``() =
    let t1 = initial len4
    let t1_1 = initial t1
    let t1_2 = initial t1_1
    
    (((length t1) = 3) && ((last t1) = "b")
    && ((length t1_1) = 2)  && ((last t1_1) = "c") 
    && ((length t1_2) = 1)  && ((last t1_2) = "d") ) |> should equal true

[<Test>]
let ``last, init, and length work test 4``() =
    let t1 = initial len5
    let t1_1 = initial t1
    let t1_2 = initial t1_1
    let t1_3 = initial t1_2
    
    (((length t1) = 4) && ((last t1) = "b")
    && ((length t1_1) = 3)  && ((last t1_1) = "c") 
    && ((length t1_2) = 2)  && ((last t1_2) = "d") 
    && ((length t1_3) = 1)  && ((last t1_3) = "e") ) |> should equal true

[<Test>]
let ``last, init, and length work test 5``() =
    let t1 = initial len6
    let t1_1 = initial t1
    let t1_2 = initial t1_1
    let t1_3 = initial t1_2
    let t1_4 = initial t1_3
    
    (((length t1) = 5) && ((last t1) = "b")
    && ((length t1_1) = 4)  && ((last t1_1) = "c") 
    && ((length t1_2) = 3)  && ((last t1_2) = "d") 
    && ((length t1_3) = 2)  && ((last t1_3) = "e") 
    && ((length t1_4) = 1)  && ((last t1_4) = "f") ) |> should equal true

[<Test>]
let ``last, init, and length work test 6``() =
    let t1 = initial len7
    let t1_1 = initial t1
    let t1_2 = initial t1_1
    let t1_3 = initial t1_2
    let t1_4 = initial t1_3
    let t1_5 = initial t1_4
    
    (((length t1) = 6) && ((last t1) = "b")
    && ((length t1_1) = 5)  && ((last t1_1) = "c") 
    && ((length t1_2) = 4)  && ((last t1_2) = "d") 
    && ((length t1_3) = 3)  && ((last t1_3) = "e") 
    && ((length t1_4) = 2)  && ((last t1_4) = "f") 
    && ((length t1_5) = 1)  && ((last t1_5) = "g") ) |> should equal true

[<Test>]
let ``last, init, and length work test 7``() =
    let t1 = initial len8
    let t1_1 = initial t1
    let t1_2 = initial t1_1
    let t1_3 = initial t1_2
    let t1_4 = initial t1_3
    let t1_5 = initial t1_4
    let t1_6 = initial t1_5
    
    (((length t1) = 7) && ((last t1) = "b") 
    && ((length t1_1) = 6)  && ((last t1_1) = "c") 
    && ((length t1_2) = 5)  && ((last t1_2) = "d") 
    && ((length t1_3) = 4)  && ((last t1_3) = "e") 
    && ((length t1_4) = 3)  && ((last t1_4) = "f") 
    && ((length t1_5) = 2)  && ((last t1_5) = "g") 
    && ((length t1_6) = 1)  && ((last t1_6) = "h") ) |> should equal true

[<Test>]
let ``last, init, and length work test 8``() =
    let t1 = initial len9
    let t1_1 = initial t1
    let t1_2 = initial t1_1
    let t1_3 = initial t1_2
    let t1_4 = initial t1_3
    let t1_5 = initial t1_4
    let t1_6 = initial t1_5
    let t1_7 = initial t1_6
    
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
    let t1 = initial lena
    let t1_1 = initial t1
    let t1_2 = initial t1_1
    let t1_3 = initial t1_2
    let t1_4 = initial t1_3
    let t1_5 = initial t1_4
    let t1_6 = initial t1_5
    let t1_7 = initial t1_6
    let t1_8 = initial t1_7
    
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
let ``unconj works``() =
    let d = ofCatLists ["f";"e";"d"] ["c";"b";"a"]
    let i1, l1 = unconj d
    let i2, l2 = unconj i1
    let i3, l3 = unconj i2
    let i4, l4 = unconj i3
    let i5, l5 = unconj i4
    let i6, l6 = unconj i5

    ((l1 = "a") && (l2 = "b") && (l3 = "c") && (l4 = "d") && (l5 = "e") && (l6 = "f") && (isEmpty i6)) |> should equal true

[<Test>]
let ``conj pattern discriminator``() =
    let d = (ofCatLists ["f";"e";"d"] ["c";"b";"a"]) 
    let i1, l1 = unconj d 

    let i2, l2 = 
        match i1 with
        | Conj(i, l) -> i, l
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
let ``cons and conj pattern discriminator``() =
    let d = (ofCatLists ["f";"e";"d"] ["c";"b";"a"]) 
    
    let mid1 = 
        match d with
        | Cons(h, Conj(i, l)) -> i
        | _ -> d

    let head, last = 
        match mid1 with
        | Cons(h, Conj(i, l)) -> h, l
        | _ -> "x", "x"

    ((head = "e") && (last = "b")) |> should equal true

[<Test>]
let ``rev deque length 1``() =
    rev len1 |> head  |> should equal "a"

[<Test>]
let ``rev deque length 2``() =
    let r1 = rev len2
    let h1 = head r1
    let t2 = tail r1
    let h2 = head t2

    ((h1 = "a")  && (h2 = "b")) |> should equal true

[<Test>]
let ``rev deque length 3``() =
    let r1 = rev len3
    let h1 = head r1
    let t2 = tail r1
    let h2 = head t2
    let t3 = tail t2
    let h3 = head t3

    ((h1 = "a") && (h2 = "b") && (h3 = "c")) |> should equal true

[<Test>]
let ``rev deque length 4``() =
    let r1 = rev len4
    let h1 = head r1
    let t2 = tail r1
    let h2 = head t2
    let t3 = tail t2
    let h3 = head t3
    let t4 = tail t3
    let h4 = head t4

    ((h1 = "a") && (h2 = "b") && (h3 = "c") && (h4 = "d")) |> should equal true

[<Test>]
let ``rev deque length 5``() =
    let r1 = rev len5
    let h1 = head r1
    let t2 = tail r1
    let h2 = head t2
    let t3 = tail t2
    let h3 = head t3
    let t4 = tail t3
    let h4 = head t4
    let t5 = tail t4
    let h5 = head t5

    ((h1 = "a") && (h2 = "b") && (h3 = "c") && (h4 = "d") && (h5 = "e")) |> should equal true

[<Test>]
//length 6 more than sufficient to test rev
let ``rev deque length 6``() =
    let r1 = rev len6
    let h1 = head r1
    let t2 = tail r1
    let h2 = head t2
    let t3 = tail t2
    let h3 = head t3
    let t4 = tail t3
    let h4 = head t4
    let t5 = tail t4
    let h5 = head t5
    let t6 = tail t5
    let h6 = head t6

    ((h1 = "a") && (h2 = "b") && (h3 = "c") && (h4 = "d") && (h5 = "e") && (h6 = "f")) |> should equal true

[<Test>]
let ``tryUncons on empty``() =
    let q = empty
    (tryUncons q = None) |> should equal true

[<Test>]
let ``tryUncons on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    let x, xs = (tryUncons q).Value 
    x |> should equal "a"

[<Test>]
let ``tryUnconj on empty``() =
    let q = empty
    (tryUnconj q = None) |> should equal true

[<Test>]
let ``tryUnconj on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    let xs, x = (tryUnconj q).Value 
    x |> should equal "d"

[<Test>]
let ``tryHead on empty``() =
    let q = empty
    (tryHead q = None) |> should equal true

[<Test>]
let ``tryHead on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    (tryHead q).Value |> should equal "a"

[<Test>]
let ``tryInitial on empty``() =
    let q = empty
    (tryInitial q = None) |> should equal true

[<Test>]
let ``tryinitial on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    let x = (tryInitial q).Value 
    let x2 = x|> last 
    x2 |> should equal "c"

[<Test>]
let ``tryLast on empty``() =
    let q = empty
    (tryLast q = None) |> should equal true

[<Test>]
let ``tryLast on deque``() =
    let q = ofSeq ["a";"b";"c";"d"]
    (tryLast q).Value |> should equal "d"
    (len2 |> tryLast).Value |> should equal "a"
    (len2conj |> tryLast).Value |> should equal "a"

[<Test>]
let ``tryTail on empty``() =
    let q = empty
    (tryTail q = None) |> should equal true

[<Test>]
let ``tryTail on q``() =
    let q = ofSeq ["a";"b";"c";"d"]
    (tryTail q).Value |> head |> should equal "b"

[<Test>]
let ``structural equality``() =

    let l1 = ofSeq [1..100]
    let l2 = ofSeq [1..100]

    l1 = l2 |> should equal true

    let l3 = ofSeq [1..99] |> conj 7

    l1 = l3 |> should equal false

//oh dear. That was a lot of work wasn't it?
//let's see if we can do better with FsCheck.
open FsCheck.Commands
//Let's do some model-based checking, as that seems to be an aspect that's underused.
type DequeActual = Deque<int>
type DequeModel = list<int>
//first, let's write the spec. What's a good model for a deque? Let's keep it simple -
//a lowly list will do. 
let spec =
    let last = List.rev >> List.head
    let tryHead l = match l with [] -> None | (h::_) -> Some h
    let tryLast = List.rev >> tryHead
    let initial = List.rev >> List.tail >> List.rev
    let check (c,m) =
        let asList = Deque.toSeq c |> Seq.toList
        m = asList //main check is that the list is the same
        && tryLast m = Deque.tryLast c //but also use other operations
        && tryHead m = Deque.tryHead c //to test the correctness of those too
        && List.length m = Deque.length c
        && List.isEmpty m = Deque.isEmpty c
    let unconj = 
        Gen.constant <|
                { new ICommand<DequeActual,DequeModel>() with
                    member x.RunActual c = Deque.unconj c |> fst
                    member x.RunModel m = initial m
                    member x.Pre m = not (List.isEmpty m)
                    member x.Post (c,m) = check (c,m)
                    override x.ToString() = sprintf "unconj"}
    let conj = 
        gen { let! elem = Arb.generate<int>
              return
                { new ICommand<DequeActual,DequeModel>() with
                    member x.RunActual c = Deque.conj elem c
                    member x.RunModel m = m @ [elem]
                    member x.Post (c,m) = check (c,m)
                    override x.ToString() = sprintf "conj %i" elem}
        }
    let uncons = 
        Gen.constant <|
                { new ICommand<DequeActual,DequeModel>() with
                    member x.RunActual c = Deque.uncons c |> snd
                    member x.RunModel m = List.tail m
                    member x.Pre m = not (List.isEmpty m)
                    member x.Post (c,m) = check (c,m)
                    override x.ToString() = sprintf "uncons"}
    let cons = 
        gen { let! elem = Arb.generate<int>
              return
                { new ICommand<DequeActual,DequeModel>() with
                    member x.RunActual c = Deque.cons elem c
                    member x.RunModel m = elem::m
                    member x.Post (c,m) = check (c,m)
                    override x.ToString() = sprintf "cons %i" elem}
        }
    let rev = 
        Gen.constant <|
                { new ICommand<DequeActual,DequeModel>() with
                    member x.RunActual c = Deque.rev c
                    member x.RunModel m = List.rev m
                    member x.Post (c,m) = check (c,m)
                    override x.ToString() = sprintf "rev"}
    { new ISpecification<DequeActual, DequeModel> with
        member x.Initial() = (Deque.empty,[])
        member x.GenCommand _ = Gen.oneof [unconj; conj; uncons; cons; rev] }

[<Test>]
let JustFsCheckIt() =
    Check.QuickThrowOnFailure (asProperty spec)