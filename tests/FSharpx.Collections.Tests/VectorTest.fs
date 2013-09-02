module FSharpx.Collections.Tests.VectorTest

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
let ``vector of vectors constructed by conjing initial``() =

    let windowFun windowLength = 
        fun (v : Vector<Vector<int>>) t ->
        if v.Last.Length = windowLength then Vector.conj (Vector.empty.Conj(t)) v
        else Vector.initial v |> Vector.conj (Vector.conj t (Vector.last v))

    let windowed = 
        seq{1..100}
        |> Seq.fold (windowFun 5) (Vector.empty.Conj Vector.empty<int>)

    windowed.Length |> should equal 20
    windowed.[2].Length |> should equal 5
        

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
let ``append works``() =
    let a = ofSeq ["a";"b";"c";"d";"e"]
    let b = ofSeq ["f";"g";"h";"i";"j"]

    append a b |> List.ofSeq |> should equal ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

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

[<Test>]
let ``toSeq to list``() =
    let l = ["f";"e";"d";"c";"b";"a"] 
    let v = ofSeq l

    v|> toSeq |> List.ofSeq |> should equal l


open FsCheck.Commands
//model-based checking
type Vector2Actual = Vector<Vector<int>>
type VectorModel = Vector<int> 

let firstVofV f (v : Vector<Vector<int>>) =
    let rec loop = function
        | x when x < v.Length ->
            if v.[x].Length > 0 then x
            else loop (x + 1)
        | _ -> 0
            
    let i = loop 0
    f i 0 

let lastVofV f (v : Vector<Vector<int>>) = 
    let rec loop = function
        | x when x > 0 ->
            if v.[x].Length > 0 then x
            else loop (x - 1)
        | _ -> 0
            
    let i = loop (v.Length - 1)
        
    let j = 
        if v.[i].Length = 0 then 0
        else v.[i].Length - 1
            
    f i j 

let lastV f (v : Vector<Vector<int>>) = 
    let rec loop = function
        | x when x > 0 ->
            if v.[x].Length > 0 then x
            else loop (x - 1)
        | _ -> 0
            
    f (loop (v.Length - 1))

let appendInnerMulti check = 
    gen { let! elems = Arb.generate<List<int>>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual c = c |> update (c.Length - 1)  (append c.[c.Length - 1] (ofSeq elems))
                member x.RunModel m = append m (ofSeq elems)
                member x.Post (c,m) = check (c,m)
                override x.ToString() = sprintf "appendInnerMulti: elems = %A" elems }
    }

let tryUpdAppendInnerMulti check = 
    gen { let! elems = Arb.generate<List<int>>
          let! elemForUpd = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual c = 
                    match (lastVofV tryUpdateNth c) elemForUpd c with
                    | Some x -> 
                        let firstUpdated = (firstVofV updateNth x) elemForUpd x
                        let last = firstUpdated.Length - 1
                        firstUpdated |> update last  (append firstUpdated.[last] (ofSeq elems))
                    | None -> c |> update (c.Length - 1)  (append c.[c.Length - 1] (ofSeq elems))
                member x.RunModel m = 
                    match tryUpdate (m.Length - 1) elemForUpd m with
                    | Some x -> append (update 0 elemForUpd x) (ofSeq elems)
                    | None -> append m (ofSeq elems)
                member x.Post (c,m) = check (c,m)
                override x.ToString() = sprintf "appendInnerMulti: elemForUpd = %i, elems = %A" elemForUpd elems }
    }

let conjInner1Elem check = 
    gen { let! elem = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual c = c |> conj (singleton elem)
                member x.RunModel m = conj elem m
                member x.Post (c,m) = check (c,m)
                override x.ToString() = sprintf "conjInner1Elem: elem = %i" elem }
    }

let tryUpdConjInner1Elem check = 
    gen { let! elem = Arb.generate<int>
          let! elemForUpd = Arb.generate<int>
          return
            { new ICommand<Vector2Actual,VectorModel>() with
                member x.RunActual c = 
                    match (firstVofV tryUpdateNth c) elemForUpd c with
                    | Some x -> 
                        (lastVofV updateNth x) elemForUpd x
                        |> conj (singleton elem)
                    | None -> c |> conj (singleton elem)
                member x.RunModel m = 
                    match tryUpdate 0 elemForUpd m with
                    | Some x -> 
                        update (x.Length - 1) elemForUpd x
                        |> conj elem
                    | None -> conj elem m
                member x.Post (c,m) = check (c,m)
                override x.ToString() = sprintf "conjInner1Elem: elem = %i, elemForUpd = %i" elem elemForUpd}
    }

let conjInnerEmpty check = 
    Gen.constant <|
        { new ICommand<Vector2Actual,VectorModel>() with
            member x.RunActual c = c |> conj empty
            member x.RunModel m = m
            member x.Post (c,m) = check (c,m) 
            override x.ToString() = sprintf "conjInnerEmpty"}

let shrinkInner check = 
    Gen.constant <|
        { new ICommand<Vector2Actual,VectorModel>() with
            member x.RunActual c = (lastV update c) ((lastV nth c) c).Initial c
            member x.RunModel m = m |> initial
            member x.Pre m = (length m) > 0
            member x.Post (c,m) = check (c,m) 
            override x.ToString() = sprintf "conjInnerEmpty"}

let checkFlatten (c,m) = 
    m = (flatten c |> ofSeq)        //main check is that the list is the same

let checkLookup (c, (m : Vector<int>)) =         //no way to distinguish which test failed
    (if m.Length = 0 then true
     else last m = (lastVofV nthNth c) c
          && nth 0 m = (firstVofV nthNth c) c)
    && tryLast m = (lastVofV tryNthNth c) c      //also use other operations
    && tryNth 0 m = (firstVofV tryNthNth c) c    //to test the correctness of those too

let specVofV genList =   
    { new ISpecification<Vector2Actual, VectorModel> with
        member x.Initial() = ((empty |> conj empty), empty)
        member x.GenCommand _ = Gen.oneof genList }

let ``Grow, check by flatten`` = [conjInner1Elem(checkFlatten); conjInnerEmpty(checkFlatten); appendInnerMulti(checkFlatten)]
let ``Grow, check by look-up`` = [conjInner1Elem(checkLookup); conjInnerEmpty(checkLookup); appendInnerMulti(checkLookup)]
let ``Grow, Update, check by flatten`` = [tryUpdConjInner1Elem(checkFlatten); conjInnerEmpty(checkFlatten); tryUpdAppendInnerMulti(checkFlatten)]
let ``Grow, Update, Shrink, check by flatten`` = [tryUpdConjInner1Elem(checkFlatten); conjInnerEmpty(checkFlatten); tryUpdAppendInnerMulti(checkFlatten); shrinkInner(checkFlatten)]

[<Test>]
let ``Grow Vector<Vector<'T>>, check by flatten``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check by flatten``))

[<Test>]
let ``Grow Vector<Vector<'T>>, check by look-up``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, check by look-up``))

[<Test>]
let ``Grow Vector<Vector<'T>>, Update, check by flatten``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, Update, check by flatten``))

[<Test>]
let ``Grow Vector<Vector<'T>>, Update, Shrink, check by flatten``() =
    Check.QuickThrowOnFailure (asProperty (specVofV ``Grow, Update, Shrink, check by flatten``))

[<Test>]
let WindowedTest() =
    let testWindowed = 
        gen { let! windowLength = Gen.choose(1,5)
              let! source = Arb.generate<List<int>>
              return ((windowSeq windowLength source), (windowLength, source))
        }
        
    Check.QuickThrowOnFailure   (Prop.forAll  (Arb.fromGen testWindowed)
                                (fun (vOfV, (windowLength, source)) -> 
                                    let outerLength =
                                        if source.Length = 0 then 1
                                        else int (Math.Ceiling((float)source.Length/(float)windowLength))
                                    (outerLength = vOfV.Length &&
                                     flatten vOfV |> List.ofSeq = source)
                                    |> Prop.classify (source.Length > 0 && outerLength > 0) "windowLength, outerLength"
                                    |> Prop.classify (source.Length = 0) "empty"
                                    |> Prop.collect (windowLength, outerLength)
                                )
                   )