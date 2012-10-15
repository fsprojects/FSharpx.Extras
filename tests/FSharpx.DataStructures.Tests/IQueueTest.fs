module FSharpx.DataStructures.Tests.IQueueTest

open FSharpx
open FSharpx.DataStructures
open FSharpx.DataStructures.Interfaces
open FSharpx.DataStructures.Tests.Infrastructure
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

let start = 1
let last = 12

let snocThruList (l : 'a list) (q : 'a IQueue)  =
    let rec loop (q' : 'a IQueue) (l' : 'a list) : 'a IQueue = 
        match l' with
        | hd :: [] -> q'.Snoc hd
        | hd :: tl -> loop (q'.Snoc hd) tl
        | [] -> q'
        
    loop q l

    (*
non- IQueue generators from random ofList
*)
let batchedQueueOfListGen =
        gen { let! x =  genListInt start last
              return ( (BatchedQueue.ofList x), x) }

let physicistQueueOfListqGen =
        gen { let! x =  genListInt start last
              return ( (PhysicistQueue.ofList x), x) }
(*
IQueue generators from random ofSeq and snoc elements from random list 
*)
let bankersQueueIntGen =
        gen { let! x =  genListInt start last
              let! y =  genListInt start last  
              return ( (BankersQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let bankersQueueIntOfSeqGen low =
        gen { let! x =  genListInt low last
              return ( (BankersQueue.ofSeq x) :> IQueue<int>, x) }

let bankersQueueIntSnocGen low =
        gen { let! x =  genListInt low last
              return ( (BankersQueue.empty() |> snocThruList x), x) }

let batchedQueueIntGen =
        gen { let! x =  genListInt start last
              let! y =  genListInt start last
              return ( (BatchedQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let batchedQueueIntOfSeqGen low =
        gen { let! x =  genListInt low last
              return ( (BatchedQueue.ofSeq x) :> IQueue<int>, x) }

let batchedQueueIntSnocGen low =
        gen { let! x =  genListInt low last
              return ( (BatchedQueue.empty() |> snocThruList x), x) }

let physicistQueueIntGen =
        gen { let! x =  genListInt start last
              let! y =  genListInt start last
              return ( (PhysicistQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let physicistQueueIntOfSeqGen low =
        gen { let! x =  genListInt low last
              return ( (PhysicistQueue.ofSeq x) :> IQueue<int>, x) }

let physicistQueueIntSnocGen low =
        gen { let! x =  genListInt low last
              return ( (PhysicistQueue.empty() |> snocThruList x), x) }

let bankersQueueObjGen =
        gen { let! x =  genListObj start last
              let! y =  genListObj start last  
              return ( (BankersQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let batchedQueueObjGen =
        gen { let! x =  genListObj start last
              let! y =  genListObj start last
              return ( (BatchedQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let physicistQueueObjGen =
        gen { let! x =  genListObj start last
              let! y =  genListObj start last
              return ( (PhysicistQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let bankersQueueStringGen =
        gen { let! x =  genListString start last
              let! y =  genListString start last  
              return ( (BankersQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let batchedQueueStringGen =
        gen { let! x =  genListString start last
              let! y =  genListString start last
              return ( (BatchedQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let physicistQueueStringGen =
        gen { let! x =  genListString start last
              let! y =  genListString start last
              return ( (PhysicistQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let emptyIQueues =
    let v = Array.create 2 (BankersQueue.empty() :> IQueue<obj>)
    v.[1] <- (BatchedQueue.empty() :> IQueue<obj>)
    //v.[2] <- (PhysicistQueue.empty() :> IQueue<obj>)
    v

// NUnit TestCaseSource does not understand array of tuples at runtime
let intGens low =
    let v = Array.create 6 (box (bankersQueueIntGen, "BankersQueue"))
    v.[1] <- box (bankersQueueIntOfSeqGen low, "BankersQueue OfSeq")
    v.[2] <- box (bankersQueueIntSnocGen low, "BankersQueue Snoc")
    v.[3] <- box (batchedQueueIntGen, "BatchedQueue")
    v.[4] <- box (batchedQueueIntOfSeqGen low, "BatchedQueue OfSeq")
    v.[5] <- box (batchedQueueIntSnocGen low, "BatchedQueue Snoc")
//    v.[6] <- box (physicistQueueIntGen, "PhysicistQueue")
//    v.[7] <- box (physicistQueueIntOfSeqGen low, "PhysicistQueue OfSeq")
//    v.[8] <- box (physicistQueueIntSnocGen low, "PhysicistQueue Snoc")
    v

let objGens =
    let v = Array.create 3 (box (bankersQueueObjGen, "BankersQueue"))
    v.[1] <- box (batchedQueueObjGen, "BatchedQueue")
    v.[2] <- box (physicistQueueObjGen, "PhysicistQueue")
    v

let stringGens =
    let v = Array.create 3 (box (bankersQueueStringGen, "BankersQueue"))
    v.[1] <- box (batchedQueueStringGen, "BatchedQueue")
    v.[2] <- box (physicistQueueStringGen, "PhysicistQueue")
    v

let intGensStart1 =
    intGens start

let intGensStart2 =
    intGens (start + 1)

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``empty queue should be empty``(eIQ : IQueue<obj>) =
    eIQ.IsEmpty() |> should equal true

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart1")>]
let ``int queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q |> Seq.toList = l |> classifyCollect q (q.Length()) ))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("objGens")>]
let ``obj queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<obj>, l) -> q |> Seq.toList = l |> classifyCollect q (q.Length()) ))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("stringGens")>]
let ``string queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<string>, l) -> q |> Seq.toList = l |> classifyCollect q (q.Length()) ))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``reverse . reverse = id``() =

    fsCheck "BankersQueue" (Prop.forAll (Arb.fromGen bankersQueueObjGen) 
        (fun (q, l) -> q :?> BankersQueue<obj> |> BankersQueue.rev |> BankersQueue.rev |> Seq.toList = (q |> Seq.toList) |> classifyCollect q (q.Length()) ))
    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen batchedQueueObjGen) 
        (fun (q, l) -> q :?> BatchedQueue<obj> |> BatchedQueue.rev |> BatchedQueue.rev |> Seq.toList = (q |> Seq.toList) |> classifyCollect q (q.Length()) ))
    //PhysicistQueue -- arbitrary structurally equal lists seem not to be comparing according to fsCheck
//    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen physicistQueueObjGen) 
//        (fun (q, l) -> q :?> PhysicistQueue<obj> |> PhysicistQueue.rev |> PhysicistQueue.rev |> Seq.toList = (q |> Seq.toList) |> classifyCollect q (q.Length()) ))

//...but I can't reproduce it outside of fscheck
//    let myList = [1;2;3;4;5;6]
//    let myList2 = myList @ myList
//
//    let outList = Seq.toList (PhysicistQueue.ofSeq myList |> snocThruList myList)
//    let outList2 = Seq.toList (PhysicistQueue.ofSeq myList |> snocThruList myList :?> PhysicistQueue<int> |> PhysicistQueue.rev |> PhysicistQueue.rev)
//
//    let y = (outList = myList2) 
//    let y2 = (outList2 = myList2) 
//    
//    y |> should equal true
//    y2 |> should equal true


[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``ofList build and serialize``() =

    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen batchedQueueOfListGen) 
        (fun ((q : BatchedQueue<int>), (l : int list)) -> q |> Seq.toList = l |> classifyCollect q (q.Length()) ))
    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen physicistQueueOfListqGen) 
        (fun ((q : PhysicistQueue<int>), (l : int list)) -> q |>  Seq.toList = l |> classifyCollect q (q.Length()) ))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``fold matches build list rev``() =

    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen batchedQueueIntGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> BatchedQueue<int> |> BatchedQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length()) ))
              
    fsCheck "BatchedQueue OfSeq" (Prop.forAll (Arb.fromGen (batchedQueueIntOfSeqGen start)) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> BatchedQueue<int> |> BatchedQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length()) ))

    fsCheck "BatchedQueue Snoc" (Prop.forAll (Arb.fromGen (batchedQueueIntSnocGen start)) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> BatchedQueue<int> |> BatchedQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length()) ))

// more problems with PhysicistQueue and fsCheck
//    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen physicistQueueIntGen) 
//        (fun ((q :IQueue<int>), (l : int list)) -> q :?> PhysicistQueue<int> |> PhysicistQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length()) ))
//              
//    fsCheck "PhysicistQueue OfSeq" (Prop.forAll (Arb.fromGen (physicistQueueIntOfSeqGen start)) 
//        (fun ((q :IQueue<int>), (l : int list)) -> q :?> PhysicistQueue<int> |> PhysicistQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length()) ))
//
//    fsCheck "PhysicistQueue Snoc" (Prop.forAll (Arb.fromGen (physicistQueueIntSnocGen start)) 
//        (fun ((q :IQueue<int>), (l : int list)) -> q :?> PhysicistQueue<int> |> PhysicistQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length()) ))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``foldback matches build list``() =

    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen batchedQueueIntGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> BatchedQueue.foldBack (fun (elem : int) (l' : int list)  -> elem::l') (q :?> BatchedQueue<int>) [] = l |> classifyCollect q (q.Length()) ))
              
    fsCheck "BatchedQueue OfSeq" (Prop.forAll (Arb.fromGen (batchedQueueIntOfSeqGen start)) 
        (fun ((q :IQueue<int>), (l : int list)) -> BatchedQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> BatchedQueue<int>) [] = l |> classifyCollect q (q.Length()) ))

    fsCheck "BatchedQueue Snoc" (Prop.forAll (Arb.fromGen (batchedQueueIntSnocGen start)) 
        (fun ((q :IQueue<int>), (l : int list)) -> BatchedQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> BatchedQueue<int>) [] = l |> classifyCollect q (q.Length()) ))

// let xs = Arb... errors in fsi and ncrunch, but not in compiled code
// To Do: create smaller demo project of the problem and submit bug report
//[<Test>]
//[<Category("nonIQueue")>]
//[<Property("Category", "nonIQueue")>]
//let ``reverse . reverse = id2``() =
//    let xs = Arb.fromGen (genListString start last)  //problem here with fsi and ncrunch
//
//    fsCheck "BankersQueue" (Prop.forAll (Arb.fromGen (genListInt start last)) (fun xs' -> 
//                                            let c = xs'.GetType().FullName.Contains("")
//                                            let q = xs' |> BankersQueue.ofSeq 
//                                            q |> BankersQueue.rev |> BankersQueue.rev |> Seq.toList = (q |> Seq.toList) 
//                                            |> classifyCollect xs' xs'.Length))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``allow to enqueue``(eIQ : IQueue<obj>) =
    ((eIQ.Snoc 1).Snoc 2).IsEmpty() |> should equal false

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``allow to dequeue``(eIQ : IQueue<obj>) =
    ((eIQ.Snoc 1).Tail()).IsEmpty() |> should equal true

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``fail if there is no head in the queue``(eIQ : IQueue<obj>) =
    let ok = ref false
    try
        eIQ.Head() |> ignore
    with x when x = Exceptions.Empty -> ok := true
    !ok |> should equal true

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``give None if there is no head in the queue``(eIQ : IQueue<obj>) =
    eIQ.TryGetHead() |> should equal None
//
[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``fail if there is no tail in the queue``(eIQ : IQueue<obj>) =
    let ok = ref false
    try
        eIQ.Tail() |> ignore
    with x when x = Exceptions.Empty -> ok := true
    !ok |> should equal true
//
[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``give None if there is no tail in the queue``(eIQ : IQueue<obj>) =
    eIQ.TryGetTail() |> should equal None

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart1")>]
let ``get the head from a queue``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q.Head() = (List.nth l 0) |> classifyCollect q (q.Length()) ))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart1")>]
let ``get the head from a queue safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q.TryGetHead().Value = (List.nth l 0) |> classifyCollect q (q.Length()) ))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart2")>]
let ``get the tail from the queue``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q.Tail().Head() = (List.nth l 1) |> classifyCollect q (q.Length()) ))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart2")>]
let ``get the tail from a queue safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q.TryGetTail().Value.Head() = (List.nth l 1) |> classifyCollect q (q.Length()) ))
