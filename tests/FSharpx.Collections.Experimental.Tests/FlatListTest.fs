module FSharpx.Collections.Experimental.Tests.FlatListTest

open System
open FSharpx.Collections.Experimental
open FSharpx.Collections.Experimental.FlatList
open FSharpx.Tests.Properties
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

(*
FlatList generators from random ofSeq and/or conj elements from random list 
*)

let flatlistIntGen =
    gen {   let! n = Gen.length1thru100
            let! x = Gen.listInt n
            return ( (FlatList.ofSeq x), x) }

let flatlistObjGen =
    gen {   let! n = Gen.length2thru100
            let! x =  Gen.listObj n
            return ( (FlatList.ofSeq x), x) }

let flatlistStringGen =
    gen {   let! n = Gen.length1thru100
            let! x =  Gen.listString n
            return ( (FlatList.ofSeq x), x) }

// NUnit TestCaseSource does not understand array of tuples at runtime
let intGens start =
    let v = Array.create 2 (box (flatlistIntGen, "FlatList"))
    v.[1] <- box ((flatlistIntGen |> Gen.suchThat (fun (v, l) -> l.Length >= start)), "FlatList OfSeq")
    v

let intGensStart1 =
    intGens 1  //this will accept all

let intGensStart2 =
    intGens 2 // this will accept 11 out of 12

[<Test>]
let ``append: multiple appends to an empty flatlist should increase the length``() =
    empty |> append (singleton 1) |> append (singleton  4) |> append (singleton  25) |> length |> should equal 3

[<Test>]
let ``append: multiple append to an empty flatlist should create a flatlist``() =
    let x = (empty |> append (singleton  1) |> append (singleton  4) |> append (singleton  25)) 
    x.[0] |> should equal 25
    x.[1] |> should equal 4
    x.[2] |> should equal 1

[<Test>]
let ``append: to an empty flatlist should create a singleton flatlist``() =
    (empty |> append (singleton 1)).[0] |> should equal 1

[<Test>]
let ``collect: expected result``() =

    fsCheck "FlatList int" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v :FlatList<int>), (l : int list)) -> v |> collect (fun elem -> ofList [ 0 .. elem ]) 
                                                       |> toList
                                                       = (l |> Array.ofList 
                                                            |> Array.collect (fun elem -> [| 0 .. elem |])
                                                            |> Array.toList) ))


[<Test>]
let ``concat: expected result``() =

    let aTable max = seq { for i in 1 .. max -> [| for j in 1 .. max -> (i, j, i*j) |] }
    let a = Array.concat (aTable 3)

    let fTable max = 
        seq { for i in 1 .. max -> [| for j in 1 .. max -> (i, j, i*j) |]}
        |> ofSeq
    let f = concat (fTable 3)

    Array.toList a |> should equal (toList f)

[<Test>]
let ``Equality: flatlist with 3 elements can be compared``() =
    let flatlist1 = ref empty
    for i in 1..3 do
        flatlist1 := append (!flatlist1) (singleton i)

    let flatlist2 = ref empty
    for i in 1..3 do
        flatlist2 := append (!flatlist2) (singleton i)

    let flatlist3 = ref empty
    for i in 1..3 do
        flatlist3 := append (!flatlist3) (singleton (2*i))


    flatlist1 = flatlist1 |> should equal true
    flatlist1 = flatlist2 |> should equal true
    flatlist1 = flatlist3 |> should equal false

[<Test>]
let ``Equality: structural equality``() =

    let l1 = ofSeq [1..100]
    let l2 = ofSeq [1..100]

    l1 = l2 |> should equal true

    let l3 = append (ofSeq [1..99]) (singleton 99)

    l1 = l3 |> should equal false

[<Test>]
let ``empty: flatlist should be empty``() =
    let x = empty<int>
    x |> length |> should equal 0

[<Test>]
let ``exists: expected result``() =

    fsCheck "FlatList int" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v :FlatList<int>), (l : int list)) -> v |> exists (fun elem -> elem = 6) 
                                                       = (l |> Array.ofList 
                                                            |> Array.exists (fun elem -> elem = 6))
                                                            ))

[<Test>]
let ``filter: expected result``() =

    fsCheck "FlatList int" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v :FlatList<int>), (l : int list)) -> v |> filter (fun elem -> elem % 2 = 0) 
                                                       |> toList
                                                       = (l |> Array.ofList 
                                                            |> Array.filter (fun elem -> elem % 2 = 0)
                                                            |> Array.toList) ))

[<Test>]
let ``fold: matches build list rev``() =

    fsCheck "FlatList int" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v :FlatList<int>), (l : int list)) -> v |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))
              
    fsCheck "FlatList obj" (Prop.forAll (Arb.fromGen flatlistObjGen) 
       (fun ((v :FlatList<obj>), (l : obj list)) -> v |> fold (fun (l' : obj list) (elem : obj) -> elem::l') [] = (List.rev l) ))

    fsCheck "FlatList string" (Prop.forAll (Arb.fromGen flatlistStringGen) 
         (fun ((v :FlatList<string>), (l : string list)) -> v |> fold (fun (l' : string list) (elem : string) -> elem::l') [] = (List.rev l) ))

[<Test>]
let ``fold2: matches build list fold``() =

    let listFun = fun (l' : (int * int) list) (elem1 : int) (elem2 : int) -> (elem1, elem2)::l'
    fsCheck "FlatList int" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v :FlatList<int>), (l : int list)) -> (v,v) ||> fold2 listFun [] = List.fold2 listFun [] l l ))
              
    let objFun = fun (l' : (obj * obj) list) (elem1 : obj) (elem2 : obj) -> (elem1, elem2)::l'
    fsCheck "FlatList obj" (Prop.forAll (Arb.fromGen flatlistObjGen) 
       (fun ((v :FlatList<obj>), (l : obj list)) -> (v,v) ||> fold2 objFun [] = List.fold2 objFun [] l l ))

    let stringFun = fun (l' : (string * string) list) (elem1 : string) (elem2 : string) -> (elem1, elem2)::l'
    fsCheck "FlatList string" (Prop.forAll (Arb.fromGen flatlistStringGen) 
         (fun ((v :FlatList<string>), (l : string list)) -> (v,v) ||> fold2 stringFun [] = List.fold2 stringFun []  l l ))

[<Test>]
let ``foldback: matches build list``() =

    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list)  -> elem::l') v [] = l ))
              
    fsCheck "FlatList obj" (Prop.forAll (Arb.fromGen flatlistObjGen) 
        (fun ((v : FlatList<obj>), (l : obj list)) -> foldBack (fun (elem : obj) (l' : obj list) -> elem::l') v [] = l ))

    fsCheck "FlatList string" (Prop.forAll (Arb.fromGen flatlistStringGen) 
        (fun ((v : FlatList<string>), (l : string list)) -> foldBack (fun (elem : string) (l' : string list) -> elem::l') v [] = l ))

[<Test>]
let ``foldback2: matches build list``() =

    let listFun = fun (elem1 : int) (elem2 : int) (l' : (int * int) list) -> (elem1, elem2)::l'
    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> foldBack2 listFun v v [] = List.foldBack2 listFun l l [] ))
              
    let objFun = fun (elem1 : obj) (elem2 : obj) (l' : (obj * obj) list) -> (elem1, elem2)::l'
    fsCheck "FlatList obj" (Prop.forAll (Arb.fromGen flatlistObjGen) 
        (fun ((v : FlatList<obj>), (l : obj list)) -> foldBack2 objFun v v [] = List.foldBack2 objFun l l [] ))

    let stringFun = fun (elem1 : string) (elem2 : string) (l' : (string * string) list) -> (elem1, elem2)::l'
    fsCheck "FlatList string" (Prop.forAll (Arb.fromGen flatlistStringGen) 
        (fun ((v : FlatList<string>), (l : string list)) -> foldBack2 stringFun v v [] = List.foldBack2 stringFun l l [] ))

[<Test>]
let ``forall: works``() =

    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> forall (fun (elem : int) -> elem < 1000) v  =  true ))

[<Test>]
let ``forall2: works``() =

    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> forall2 (fun (elem1 : int) (elem2 : int) -> (elem1 < 1000 && elem2 < 1000)) v v  =  true ))

[<Test>]
let ``GetHashCode: flatlist with 3 elements can compute hashcodes``() =
    let flatlist1 = ref empty
    for i in 1..3 do
        flatlist1 := append (!flatlist1) (singleton i) 

    let flatlist2 = ref empty
    for i in 1..3 do
        flatlist2 := append (!flatlist2) (singleton i)

    let flatlist3 = ref empty
    for i in 1..3 do
        flatlist3 := append (!flatlist3) (singleton (2*i))

    flatlist1.GetHashCode() |> should equal (flatlist2.GetHashCode())
    ((flatlist1.GetHashCode()) = (flatlist3.GetHashCode())) |> should equal false

[<Test>]
let ``init: flatlist should allow init``() =
    let flatlist = init 5 (fun x -> x * 2) 
    let s = Seq.init 5 (fun x -> x * 2)

    s |> Seq.toList |> should equal [0;2;4;6;8]
    flatlist |> Seq.toList |> should equal [0;2;4;6;8]

[<Test>]
let ``IEnumarable: flatlist with 300 elements should be convertable to a seq``() =
    let flatlist = ref empty
    for i in 1..300 do
        flatlist := append (!flatlist) (singleton i) 

    !flatlist |> Seq.toList |> should equal [1..300]

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``Item: get last from flatlist``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (v : FlatList<int>, l : list<int>) -> v.[l.Length - 1] = (List.nth l (l.Length - 1)) ))

[<Test>]
let ``iter: flatlist should allow iter``() =

    let l' = ref []
   
    let l2 = [1;2;3;4]
    let v = ofSeq l2

    iter (fun (elem : int) -> l' := elem::!l') v
    
    !l' |> should equal (List.rev l2)                          

[<Test>]
let ``iter2: flatlist should allow iter2``() =

    let l' = ref []
   
    let l2 = [1;2;3;4]
    let v = ofSeq l2

    iter2 (fun (elem1 : int) (elem2 : int) -> l' := elem1::elem2::!l') v v
    
    !l' |> should equal (List.rev [1;1;2;2;3;3;4;4])        

[<Test>]
let ``iteri: flatlist should allow iteri``() =

    let l' = ref []
   
    let l2 = [1;2;3;4]
    let v = ofSeq l2

    iteri (fun i (elem : int) -> l' := (i * elem)::!l') v
    
    !l' |> should equal (List.rev [0;2;6;12])   


[<Test>]
let ``map: flatlist should allow map``() =

    let funMap = (fun x ->  x * 2)
    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> map funMap v |> toList =  List.map funMap l ))

[<Test>]
let ``map2: flatlist should allow map2``() =

    let funMap2 = (fun x y ->  ((x * 2), (y * 2)))
    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> map2 funMap2 v v |> toList =  List.map2 funMap2 l l ))

[<Test>]
let ``mapi: flatlist should allow mapi``() =

    let funMapi = (fun i x ->  i * x)
    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> mapi funMapi v |> toList =  List.mapi funMapi l ))

[<Test>]
let ``ofList: flatlist can be created``() =
    let xs = [7;88;1;4;25;30] 
    ofList xs |> Seq.toList |> should equal xs

[<Test>]
let ``ofSeq: flatlist can be created``() =
    let xs = [7;88;1;4;25;30] 
    ofSeq xs |> Seq.toList |> should equal xs

[<Test>]
let ``partition: works``() =

    let funMapi = (fun x ->  x % 2 = 0)
    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> let x, y = partition funMapi v 
                                                      ((toList x),(toList y)) =  List.partition funMapi l ))

[<Test>]
let ``physicalEquality: works``() =

    let l1 = ofSeq [1..100]
    let l2 = l1
    let l3 = ofSeq [1..100]

    physicalEquality l1 l2 |> should equal true

    physicalEquality l1 l3 |> should equal false

[<Test>]
let ``rev: empty``() =
    isEmpty (empty |> rev) |> should equal true
    
[<Test>]
let ``rev: matches build list rev``() =

    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((q :FlatList<int>), (l : int list)) -> q |> rev |> List.ofSeq = (List.rev l) ))
              
    fsCheck "FlatList obj" (Prop.forAll (Arb.fromGen flatlistObjGen) 
        (fun ((q :FlatList<obj>), (l : obj list)) -> q |> rev |> List.ofSeq = (List.rev l) ))

    fsCheck "FlatList string" (Prop.forAll (Arb.fromGen flatlistStringGen) 
         (fun ((q :FlatList<string>), (l : string list)) -> q |> rev |> List.ofSeq = (List.rev l) ))

[<Test>]
let ``sum: works``() =

    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> sum v = List.sum l ))

[<Test>]
let ``sumBy: works``() =

    let funSumBy = (fun x ->  x * 2)
    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> sumBy funSumBy v = List.sumBy funSumBy l ))

[<Test>]
let ``toMap: works``() =
  
    let l2 = [(1,"a");(2,"b");(3,"c");(4,"d")]
    let v = ofList l2

    let m = toMap v
    
    m.[1] |> should equal "a"  
    m.[2] |> should equal "b"
    m.[3] |> should equal "c"
    m.[4] |> should equal "d" 
    m.ContainsKey 5 |> should equal false

[<Test>]
let ``tryFind: works``() =

    let funTryFind = (fun x ->  x % 2 = 0)
    fsCheck "FlatList" (Prop.forAll (Arb.fromGen flatlistIntGen) 
        (fun ((v : FlatList<int>), (l : int list)) -> 
                                                    match tryFind funTryFind v with
                                                    | None -> None = List.tryFind funTryFind l 
                                                    | Some x -> x = (List.tryFind funTryFind l).Value  ))

[<Test>]
let ``unzip: works``() =
  
    let l2 = [(1,"a");(2,"b");(3,"c");(4,"d")]
    let v = ofList l2
    let x, y = unzip v
    
    toList x |> should equal [1;2;3;4]  
    toList y |> should equal ["a";"b";"c";"d"]

[<Test>]
let ``zip: works``() =
  
    let l1 = [1;2;3;4]
    let l2 = ["a";"b";"c";"d"]
    let v1 = ofList l1
    let v2 = ofList l2
    let v = zip v1 v2
    
    toList v |> should equal [(1,"a");(2,"b");(3,"c");(4,"d")]  