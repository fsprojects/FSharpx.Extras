module FSharpx.Tests.VectorTest

open System
open FSharpx.Collections
open FSharpx.Collections.Vector
open FSharpx.Tests.Properties
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

let conjThruList l v  =
    let rec loop (v' : 'a Vector) (l' : 'a list) = 
        match l' with
        | hd :: [] -> v'.Conj hd
        | hd :: tl -> loop (v'.Conj hd) tl
        | [] -> v'
        
    loop v l

(*
Vector generators from random ofSeq and/or conj elements from random list 
//vector blocksizej of 32, need to generate lists up to 100
*)
let vectorIntGen =
    gen {   let! n = Gen.length1thru100
            let! n2 = Gen.length2thru100
            let! x =  Gen.listInt n
            let! y =  Gen.listInt n2
            return ( (Vector.ofSeq x |> conjThruList y), (x @ y) ) }

let vectorIntOfSeqGen =
    gen {   let! n = Gen.length1thru100
            let! x = Gen.listInt n
            return ( (Vector.ofSeq x), x) }

let vectorIntConjGen =
    gen {   let! n = Gen.length1thru100
            let! x = Gen.listInt n
            return ( (Vector.empty |> conjThruList x), x) }

let vectorObjGen =
    gen {   let! n = Gen.length2thru100
            let! n2 = Gen.length1thru100
            let! x =  Gen.listObj n
            let! y =  Gen.listObj n2
            return ( (Vector.ofSeq x |> conjThruList y), (x @ y) ) }

let vectorStringGen =
    gen {   let! n = Gen.length1thru100
            let! n2 = Gen.length2thru100
            let! x =  Gen.listString n
            let! y =  Gen.listString n2  
            return ( (Vector.ofSeq x |> conjThruList y), (x @ y) ) }

// NUnit TestCaseSource does not understand array of tuples at runtime
let intGens start =
    let v = Array.create 3 (box (vectorIntGen, "Vector"))
    v.[1] <- box ((vectorIntOfSeqGen |> Gen.suchThat (fun (v, l) -> l.Length >= start)), "Vector OfSeq")
    v.[2] <- box ((vectorIntConjGen |> Gen.suchThat (fun (v, l) -> l.Length >= start)), "Vector Envector") 
    v

let intGensStart1 =
    intGens 1  //this will accept all

let intGensStart2 =
    intGens 2 // this will accept 11 out of 12

[<Test>]
let ``fold matches build list rev``() =

    fsCheck "Vector" (Prop.forAll (Arb.fromGen vectorIntGen) 
        (fun ((v :Vector<int>), (l : int list)) -> v |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))
              
    fsCheck "Vector OfSeq" (Prop.forAll (Arb.fromGen vectorIntOfSeqGen) 
       (fun ((v :Vector<int>), (l : int list)) -> v |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

    fsCheck "Vector Conj" (Prop.forAll (Arb.fromGen vectorIntConjGen) 
         (fun ((v :Vector<int>), (l : int list)) -> v |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

[<Test>]
let ``foldback matches build list``() =

    fsCheck "Vector" (Prop.forAll (Arb.fromGen vectorIntGen) 
        (fun ((v : Vector<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list)  -> elem::l') v [] = l ))
              
    fsCheck "Vector OfSeq" (Prop.forAll (Arb.fromGen vectorIntOfSeqGen) 
        (fun ((v : Vector<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') v [] = l ))

    fsCheck "Vector Conj" (Prop.forAll (Arb.fromGen vectorIntConjGen) 
        (fun ((v : Vector<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') v [] = l ))

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``get last from vector``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (v : Vector<int>, l) -> (last v) = (List.nth l (l.Length - 1)) ))

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``get last from vector safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (v : Vector<int>, l) -> (tryLast v).Value = (List.nth l (l.Length - 1)) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get initial from vector``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((v : Vector<int>), l) -> v.Initial.Last = (List.nth l (l.Length - 2)) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get initial from vector safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (v : Vector<int>, l) -> v.TryInitial.Value.Last = (List.nth l (l.Length - 2)) ))

[<Test>]
let ``empty vector should be empty``() =
    let x = empty<int>
    x |> length |> should equal 0

[<Test>]
let ``multiple cons to an empty vector should increase the length``() =
    empty |> conj 1 |> conj 4 |> conj 25 |> length |> should equal 3

[<Test>]
let ``cons to an empty vector should create a singleton vector``() =
    empty |> conj 1 |> nth 0 |> should equal 1

[<Test>]
let ``multiple cons to an empty vector should create a vector``() =
    empty |> conj 1 |> conj 4 |> conj 25 |> nth 1 |> should equal 4

[<Test>]
let ``multiple assoc to the end should work like cons and create a vector``() =
    let v = empty |> update 0 1 |> update 1 4 |> update 2 25 
    v |> nth 0 |> should equal 1
    v |> nth 1 |> should equal 4
    v |> nth 2 |> should equal 25

[<Test>]
let ``300 cons to an empty vector should create a vector``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    !vector |> nth 100 |> should equal 101
    !vector |> nth 200 |> should equal 201

[<Test>]
let ``assoc an element to a nonempty vector should not change the original vector``() =
    let v = empty<string> |> conj "1" |> conj "4" |> conj "25" 

    v |> update 2 "5" |> nth 2 |> should equal "5"
    v |> nth 2 |> should equal "25"

[<Test>]
let ``tryupdate of long vector``() =
    let v = ofSeq [1..100]

    v |> update 99 5 |> nth 99 |> should equal 5

[<Test>]
let ``vector should should be convertable to a seq``() =
    empty |> conj 1 |> conj 4 |> conj 25  |> Seq.toList |> should equal [1;4;25]

[<Test>]
let ``vector with 300 elements should be convertable to a seq``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    !vector |> Seq.toList |> should equal [1..300]

[<Test>]
let ``vector can be created from a seq``() =
    let xs = [7;88;1;4;25;30] 
    ofSeq xs |> Seq.toList |> should equal xs

[<Test>]
let ``vector with 300 elements should allow update``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    for i in 1..300 do
        vector := update (i-1) (i*2) (!vector)

    !vector |> Seq.toList |> should equal [for i in 1..300 -> i*2]

[<Test>]
let ``access last element from a vector``() =
    let vector = empty |> conj 1 |> conj 4 |> conj 25 
    vector |> last |> should equal 25
    
[<Test>]
let ``can get initial elements from a vector``() =
    let vector = empty |> conj 1 |> conj 4 |> conj 25 
    vector |> last |> should equal 25
    vector |> initial |> last |> should equal 4
    vector |> initial |> initial |> last |> should equal 1

    vector |> length |> should equal 3
    vector |> initial |> length |> should equal 2
    vector |> initial |> initial |> length |> should equal 1

[<Test>]
let ``vector with 300 elements should allow initial``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    for i in 1..300 do
        vector := initial (!vector)

    !vector |> Seq.toList |> should equal []

[<Test>]
let ``vector with 3 elements can compute hashcodes``() =
    let vector1 = ref empty
    for i in 1..3 do
        vector1 := conj i (!vector1)

    let vector2 = ref empty
    for i in 1..3 do
        vector2 := conj i (!vector2)

    let vector3 = ref empty
    for i in 1..3 do
        vector3 := conj (2*i) (!vector3)

    vector1.GetHashCode() |> should equal (vector2.GetHashCode())
    vector1.GetHashCode() |> should equal (vector2.GetHashCode())

[<Test>]
let ``vector with 3 elements can be compared``() =
    let vector1 = ref empty
    for i in 1..3 do
        vector1 := conj i (!vector1)

    let vector2 = ref empty
    for i in 1..3 do
        vector2 := conj i (!vector2)

    let vector3 = ref empty
    for i in 1..3 do
        vector3 := conj (2*i) (!vector3)


    vector1 = vector1 |> should equal true
    vector1 = vector2 |> should equal true
    vector1 = vector3 |> should equal false

[<Test>]
let ``vector should allow map``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    let vector2 = map (fun x -> x * 2) (!vector)

    vector2 |> Seq.toList |> should equal [for i in 1..300 -> i * 2]

[<Test>]
let ``vector should allow init``() =
    let vector = init 5 (fun x -> x * 2) 
    let s = Seq.init 5 (fun x -> x * 2)

    s |> Seq.toList |> should equal [0;2;4;6;8]
    vector |> Seq.toList |> should equal [0;2;4;6;8]

[<Test>]
let ``conj pattern discriminator - Vector``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"]
    
    let t1, h1 = 
        match q with
        | Conj(t, h) -> t, h
        | _ ->  q, "x"

    ((h1 = "a") && (t1.Length = 5)) |> should equal true

[<Test>]
let ``TryUnconj wind-down to None``() =
    let v = ofSeq ["f";"e";"d";"c";"b";"a"] 

    let rec loop (v' : Vector<string>) = 
        match (v'.TryUnconj) with
        | Some(initl, lst) ->  loop initl
        | None -> ()

    loop v

    true |> should equal true

[<Test>]
let ``Unconj wind-down to None``() =
    let v = ofSeq ["f";"e";"d";"c";"b";"a"] 

    let rec loop (v' : Vector<string>) = 
        match (v'.Unconj) with
        | initl, lst when initl.IsEmpty ->  ()
        | initl, lst ->  loop initl

    loop v

    true |> should equal true

[<Test>]
let ``rev empty``() =
    isEmpty (empty |> rev) |> should equal true
    
[<Test>]
let ``rev elements length 5``() =
    let a = ofSeq ["a";"b";"c";"d";"e"]

    let b = rev a

    let c = List.ofSeq b
    a.Last |> should equal "e"
    b.Last |> should equal "a"
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

    fsCheck "Vector" (Prop.forAll (Arb.fromGen vectorIntGen) 
        (fun ((q :Vector<int>), (l : int list)) -> q |> rev |> List.ofSeq = (List.rev l) ))
              
    fsCheck "Vector OfSeq" (Prop.forAll (Arb.fromGen vectorIntOfSeqGen) 
        (fun ((q :Vector<int>), (l : int list)) -> q |> rev |> List.ofSeq = (List.rev l) ))

    fsCheck "Vector Cons" (Prop.forAll (Arb.fromGen vectorIntConjGen) 
         (fun ((q :Vector<int>), (l : int list)) -> q |> rev |> List.ofSeq = (List.rev l) ))

[<Test>]
let ``structural equality``() =

    let l1 = ofSeq [1..100]
    let l2 = ofSeq [1..100]

    l1 = l2 |> should equal true

    let l3 = l2 |> update 98 7

    l1 = l3 |> should equal false