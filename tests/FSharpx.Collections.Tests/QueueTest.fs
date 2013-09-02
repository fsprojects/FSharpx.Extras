module FSharpx.Collections.Tests.QueueTest

open FSharpx.Collections
open FSharpx.Collections.Queue
open FSharpx.Tests.Properties
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

let emptyQueue = Queue.empty

let enqueueThruList l q  =
    let rec loop (q' : 'a Queue) (l' : 'a list) = 
        match l' with
        | hd :: [] -> q'.Conj hd
        | hd :: tl -> loop (q'.Conj hd) tl
        | [] -> q'
        
    loop q l

//Queue
(*
non-IQueue generators from random ofList
*)
let queueOfListGen =
    gen {   let! n = Gen.length2thru12
            let! x = Gen.listInt n
            return ( (Queue.ofList x), x) }

(*
IQueue generators from random ofSeq and/or conj elements from random list 
*)
let queueIntGen =
    gen {   let! n = Gen.length1thru12
            let! n2 = Gen.length2thru12
            let! x =  Gen.listInt n
            let! y =  Gen.listInt n2
            return ( (Queue.ofSeq x |> enqueueThruList y), (x @ y) ) }

let queueIntOfSeqGen =
    gen {   let! n = Gen.length1thru12
            let! x = Gen.listInt n
            return ( (Queue.ofSeq x), x) }

let queueIntConjGen =
    gen {   let! n = Gen.length1thru12
            let! x = Gen.listInt n
            return ( (Queue.empty |> enqueueThruList x), x) }

let queueObjGen =
    gen {   let! n = Gen.length2thru12
            let! n2 = Gen.length1thru12
            let! x =  Gen.listObj n
            let! y =  Gen.listObj n2
            return ( (Queue.ofSeq x |> enqueueThruList y), (x @ y) ) }

let queueStringGen =
    gen {   let! n = Gen.length1thru12
            let! n2 = Gen.length2thru12
            let! x =  Gen.listString n
            let! y =  Gen.listString n2  
            return ( (Queue.ofSeq x |> enqueueThruList y), (x @ y) ) }

// NUnit TestCaseSource does not understand array of tuples at runtime
let intGens start =
    let v = Array.create 3 (box (queueIntGen, "Queue"))
    v.[1] <- box ((queueIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "Queue OfSeq")
    v.[2] <- box ((queueIntConjGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "Queue Enqueue") 
    v

let intGensStart1 =
    intGens 1  //this will accept all

let intGensStart2 =
    intGens 2 // this will accept 11 out of 12

[<Test>]
let ``allow to dequeue``() =
    emptyQueue |> conj 1 |> tail |> isEmpty |> should equal true

[<Test>]
let ``allow to enqueue``() =
    emptyQueue |> conj 1 |> conj 2 |> isEmpty |> should equal false

[<Test>]
let ``cons pattern discriminator - Queue``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"]
    
    let h1, t1 = 
        match q with
        | Cons(h, t) -> h, t
        | _ ->  "x", q

    ((h1 = "f") && (t1.Length = 5)) |> should equal true

[<Test>]
let ``empty queue should be empty``() =
    emptyQueue |> isEmpty |> should equal true

[<Test>]
let ``fail if there is no head in the queue``() =
    (fun () -> emptyQueue |> head |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``fail if there is no tail in the queue``() =
    (fun () -> emptyQueue |> tail |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``fold matches build list rev``() =

    fsCheck "Queue" (Prop.forAll (Arb.fromGen queueIntGen) 
        (fun ((q :Queue<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q q.Length))
              
    fsCheck "Queue OfSeq" (Prop.forAll (Arb.fromGen queueIntOfSeqGen) 
//        (fun ((q :Queue<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q q.Length))
//executes faster; classifyCollect for debugging
        (fun ((q :Queue<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

    fsCheck "Queue Conj" (Prop.forAll (Arb.fromGen queueIntConjGen) 
//        (fun ((q :Queue<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q q.Length))
         (fun ((q :Queue<int>), (l : int list)) -> q |> fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) ))

[<Test>]
let ``foldback matches build list``() =

    fsCheck "Queue" (Prop.forAll (Arb.fromGen queueIntGen) 
//        (fun ((q : Queue<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list)  -> elem::l') q [] = l |> classifyCollect q q.Length))
        (fun ((q : Queue<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list)  -> elem::l') q [] = l ))
              
    fsCheck "Queue OfSeq" (Prop.forAll (Arb.fromGen queueIntOfSeqGen) 
//        (fun ((q : Queue<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l |> classifyCollect q q.Length))
        (fun ((q : Queue<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l |> classifyCollect q q.Length))

    fsCheck "Queue Conj" (Prop.forAll (Arb.fromGen queueIntConjGen) 
//        (fun ((q : Queue<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l |> classifyCollect q q.Length))
        (fun ((q : Queue<int>), (l : int list)) -> foldBack (fun (elem : int) (l' : int list) -> elem::l') q [] = l ))

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``get head from queue``(x : obj) =
    let genAndName = unbox x 
//    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Queue<int>, l) -> (peek q) = (List.nth l 0) |> classifyCollect q q.Length))
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Queue<int>, l) -> (head q) = (List.nth l 0) ))

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``get head from queue safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Queue<int>, l) -> (tryHead q).Value = (List.nth l 0) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get tail from queue``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((q : Queue<int>), l) -> q.Tail.Head = (List.nth l 1) ))

[<Test>]
[<TestCaseSource("intGensStart2")>]
let ``get tail from queue safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Queue<int>, l) -> q.TryTail.Value.Head = (List.nth l 1) ))

[<Test>]
let ``give None if there is no head in the queue``() =
    emptyQueue |> tryHead |> should equal None

[<Test>]
let ``give None if there is no tail in the queue``() =
    emptyQueue |> tryTail |> should equal None

[<Test>]
[<TestCaseSource("intGensStart1")>]
let ``int queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : Queue<int>, l) -> q |> Seq.toList = l ))

[<Test>]
let ``obj queue builds and serializes``() =
    fsCheck "obj Queue" (Prop.forAll (Arb.fromGen queueObjGen) (fun (q : Queue<obj>, l) -> q |> Seq.toList = l ))

[<Test>]
let ``string queue builds and serializes``() =
    fsCheck "string Queue" (Prop.forAll (Arb.fromGen queueStringGen) (fun (q : Queue<string>, l) -> q |> Seq.toList = l ))

[<Test>]
let ``reverse . reverse = id``() =
    
    fsCheck "obj Queue" (Prop.forAll (Arb.fromGen queueObjGen) 
        (fun (q, l) -> q |> rev |> rev |> Seq.toList = (q |> Seq.toList) ))
    
[<Test>]
let ``ofList build and serialize``() =

    fsCheck "Queue" (Prop.forAll (Arb.fromGen queueOfListGen) 
        (fun ((q : Queue<int>), (l : int list)) -> q |> Seq.toList = l ))

[<Test>]
let ``toSeq to list``() =
    let l = ["f";"e";"d";"c";"b";"a"] 
    let q = ofSeq l

    q|> toSeq |> List.ofSeq |> should equal l

[<Test>]
let ``TryUncons wind-down to None``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"] 

    let rec loop (q' : Queue<string>) = 
        match (q'.TryUncons) with
        | Some(hd, tl) ->  loop tl
        | None -> ()

    loop q

    true |> should equal true

[<Test>]
let ``Uncons wind-down to None``() =
    let q = ofSeq ["f";"e";"d";"c";"b";"a"] 

    let rec loop (q' : Queue<string>) = 
        match (q'.Uncons) with
        | hd, tl when tl.IsEmpty ->  ()
        | hd, tl ->  loop tl

    loop q

    true |> should equal true

[<Test>]
let ``structural equality``() =

    let l1 = ofSeq [1..100]
    let l2 = ofSeq [1..100]

    l1 = l2 |> should equal true

    let l3 = ofSeq [1..99] |> conj 7

    l1 = l3 |> should equal false