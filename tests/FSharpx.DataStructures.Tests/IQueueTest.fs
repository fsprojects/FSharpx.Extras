module FSharpx.DataStructures.Tests.IQueueTest

open FSharpx
open FSharpx.DataStructures
open FSharpx.DataStructures.Interfaces
open FSharpx.Tests.Properties
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

let snocThruList l q  =
    let rec loop (q' : 'a IQueue) (l' : 'a list) = 
        match l' with
        | hd :: [] -> q'.Snoc hd
        | hd :: tl -> loop (q'.Snoc hd) tl
        | [] -> q'
        
    loop q l

let length1thru12 = Gen.choose (1, 12)
let length2thru12 = Gen.choose (2, 12)
(*
non- IQueue generators from random ofList
*)
let batchedQueueOfListGen =
        gen { let! n = length2thru12
              let! x = Gen.listInt n
              return ( (BatchedQueue.ofList x), x) }

let physicistQueueOfListqGen =
        gen { let! n = length2thru12
              let! x = Gen.listInt n
              return ( (PhysicistQueue.ofList x), x) }
(*
IQueue generators from random ofSeq and snoc elements from random list 
*)
let bankersQueueIntGen =
        gen { let! n = length1thru12
              let! n2 = length1thru12
              let! x = Gen.listInt n
              let! y = Gen.listInt n2  
              return ( (BankersQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let bankersQueueIntOfSeqGen =
        gen { let! n = length2thru12
              let! n = length2thru12
              let! x = Gen.listInt n
              return ( (BankersQueue.ofSeq x) :> IQueue<int>, x) }

let bankersQueueIntSnocGen =
        gen { let! n = length2thru12
              let! x = Gen.listInt n
              return ( (BankersQueue.empty() |> snocThruList x), x) }

let batchedQueueIntGen =
        gen { let! n = length1thru12
              let! n2 = length1thru12
              let! x =  Gen.listInt n
              let! y =  Gen.listInt n2
              return ( (BatchedQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let batchedQueueIntOfSeqGen =
        gen { let! n = length2thru12
              let! x = Gen.listInt n
              return ( (BatchedQueue.ofSeq x) :> IQueue<int>, x) }

let batchedQueueIntSnocGen =
        gen { let! n = length2thru12
              let! x = Gen.listInt n
              return ( (BatchedQueue.empty() |> snocThruList x), x) }

let physicistQueueIntGen =
        gen { let! n = length1thru12
              let! n2 = length1thru12
              let! x = Gen.listInt n
              let! y = Gen.listInt n2
              return ( (PhysicistQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let physicistQueueIntOfSeqGen =
        gen { let! n = length2thru12
              let! x = Gen.listInt n
              return ( (PhysicistQueue.ofSeq x) :> IQueue<int>, x) }

let physicistQueueIntSnocGen =
        gen { let! n = length2thru12
              let! x = Gen.listInt n
              return ( (PhysicistQueue.empty() |> snocThruList x), x) }

let bankersQueueObjGen =
        gen { let! n = length1thru12
              let! n2 = length1thru12
              let! x =  Gen.listObj n
              let! y =  Gen.listObj n2  
              return ( (BankersQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let batchedQueueObjGen =
        gen { let! n = length1thru12
              let! n2 = length1thru12
              let! x =  Gen.listObj n
              let! y =  Gen.listObj n2
              return ( (BatchedQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let physicistQueueObjGen =
        gen { let! n = length1thru12
              let! n2 = length1thru12
              let! x =  Gen.listObj n
              let! y =  Gen.listObj n2
              return ( (PhysicistQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let bankersQueueStringGen =
        gen { let! n = length1thru12
              let! n2 = length1thru12
              let! x =  Gen.listString n
              let! y =  Gen.listString n2  
              return ( (BankersQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let batchedQueueStringGen =
        gen { let! n = length1thru12
              let! n2 = length1thru12
              let! x =  Gen.listString n
              let! y =  Gen.listString n2  
              return ( (BatchedQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let physicistQueueStringGen =
        gen { let! n = length1thru12
              let! n2 = length1thru12
              let! x =  Gen.listString n
              let! y =  Gen.listString n2  
              return ( (PhysicistQueue.ofSeq x |> snocThruList y), (x @ y) ) }

let emptyIQueues =
    let v = Array.create 3 (BankersQueue.empty() :> IQueue<obj>)
    v.[1] <- (BatchedQueue.empty() :> IQueue<obj>)
    v.[2] <- (PhysicistQueue.empty() :> IQueue<obj>)
    v

// NUnit TestCaseSource does not understand array of tuples at runtime
let intGens start =
    let v = Array.create 9 (box (bankersQueueIntGen, "BankersQueue"))
    v.[1] <- box ((bankersQueueIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "BankersQueue OfSeq")
    v.[2] <- box ((bankersQueueIntSnocGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "BankersQueue Snoc")
    v.[3] <- box (batchedQueueIntGen, "BatchedQueue")
    v.[4] <- box ((batchedQueueIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "BatchedQueue OfSeq")
    v.[5] <- box ((batchedQueueIntSnocGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "BatchedQueue Snoc")
    v.[6] <- box (physicistQueueIntGen, "PhysicistQueue")
    v.[7] <- box ((physicistQueueIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "PhysicistQueue OfSeq")
    v.[8] <- box ((physicistQueueIntSnocGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "PhysicistQueue Snoc")
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
    intGens 1  //this will accept all

let intGensStart2 =
    intGens 2 // this will accept 11 out of 12

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``allow to dequeue``(eIQ : IQueue<obj>) =
    ((eIQ.Snoc 1).Tail).IsEmpty |> should equal true

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``allow to enqueue``(eIQ : IQueue<obj>) =
    ((eIQ.Snoc 1).Snoc 2).IsEmpty |> should equal false

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``cons pattern discriminator - BankersQueue``() =
    let q = BankersQueue.ofSeq  ["f";"e";"d";"c";"b";"a"]
    
    let h1, t1 = 
        match q with
        | BankersQueue.Cons(h, t) -> h, t
        | _ ->  "x", q

    ((h1 = "f") && (t1.Length = 5)) |> should equal true

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``cons pattern discriminator - BatchedQueue``() =
    let q = BatchedQueue.ofSeq  ["f";"e";"d";"c";"b";"a"]
    
    let h1, t1 = 
        match q with
        | BatchedQueue.Cons(h, t) -> h, t
        | _ ->  "x", q

    ((h1 = "f") && (t1.Length = 5)) |> should equal true

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``cons pattern discriminator - PhysicistQueue``() =
    let q = PhysicistQueue.ofSeq  ["f";"e";"d";"c";"b";"a"]
    
    let h1, t1 = 
        match q with
        | PhysicistQueue.Cons(h, t) -> h, t
        | _ ->  "x", q

    ((h1 = "f") && (t1.Length = 5)) |> should equal true

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``empty queue should be empty``(eIQ : IQueue<obj>) =
    eIQ.IsEmpty |> should equal true

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``fail if there is no head in the queue``(eIQ : IQueue<obj>) =
    let ok = ref false
    try
        eIQ.Head |> ignore
    with x when x = Exceptions.Empty -> ok := true
    !ok |> should equal true

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``fail if there is no tail in the queue``(eIQ : IQueue<obj>) =
    let ok = ref false
    try
        eIQ.Tail |> ignore
    with x when x = Exceptions.Empty -> ok := true
    !ok |> should equal true

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart1")>]
let ``int queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q |> Seq.toList = l |> classifyCollect q q.Length))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("objGens")>]
let ``obj queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<obj>, l) -> q |> Seq.toList = l |> classifyCollect q q.Length))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("stringGens")>]
let ``string queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<string>, l) -> q |> Seq.toList = l |> classifyCollect q q.Length))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``reverse . reverse = id``() =

    fsCheck "BankersQueue" (Prop.forAll (Arb.fromGen bankersQueueObjGen) 
        (fun (q, l) -> q :?> BankersQueue<obj> |> BankersQueue.rev |> BankersQueue.rev |> Seq.toList = (q |> Seq.toList) |> classifyCollect q q.Length))
    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen batchedQueueObjGen) 
        (fun (q, l) -> q :?> BatchedQueue<obj> |> BatchedQueue.rev |> BatchedQueue.rev |> Seq.toList = (q |> Seq.toList) |> classifyCollect q q.Length))
    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen physicistQueueIntGen) 
        (fun (q, l) -> q :?> PhysicistQueue<int> |> PhysicistQueue.rev |> PhysicistQueue.rev |> Seq.toList = (q |> Seq.toList) |> classifyCollect q q.Length))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``ofList build and serialize``() =

    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen batchedQueueOfListGen) 
        (fun ((q : BatchedQueue<int>), (l : int list)) -> q |> Seq.toList = l |> classifyCollect q q.Length))
    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen physicistQueueOfListqGen) 
        (fun ((q : PhysicistQueue<int>), (l : int list)) -> q |>  Seq.toList = l |> classifyCollect q q.Length))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``fold matches build list rev``() =

    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen batchedQueueIntGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> BatchedQueue<int> |> BatchedQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q q.Length))
              
    fsCheck "BatchedQueue OfSeq" (Prop.forAll (Arb.fromGen batchedQueueIntOfSeqGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> BatchedQueue<int> |> BatchedQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q q.Length))

    fsCheck "BatchedQueue Snoc" (Prop.forAll (Arb.fromGen batchedQueueIntSnocGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> BatchedQueue<int> |> BatchedQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q q.Length))

    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen physicistQueueIntGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> PhysicistQueue<int> |> PhysicistQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q q.Length))
              
    fsCheck "PhysicistQueue OfSeq" (Prop.forAll (Arb.fromGen (physicistQueueIntOfSeqGen)) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> PhysicistQueue<int> |> PhysicistQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q q.Length))

    fsCheck "PhysicistQueue Snoc" (Prop.forAll (Arb.fromGen (physicistQueueIntSnocGen)) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> PhysicistQueue<int> |> PhysicistQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q q.Length))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``foldback matches build list``() =

    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen batchedQueueIntGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> BatchedQueue.foldBack (fun (elem : int) (l' : int list)  -> elem::l') (q :?> BatchedQueue<int>) [] = l |> classifyCollect q q.Length))
              
    fsCheck "BatchedQueue OfSeq" (Prop.forAll (Arb.fromGen batchedQueueIntOfSeqGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> BatchedQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> BatchedQueue<int>) [] = l |> classifyCollect q q.Length))

    fsCheck "BatchedQueue Snoc" (Prop.forAll (Arb.fromGen batchedQueueIntSnocGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> BatchedQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> BatchedQueue<int>) [] = l |> classifyCollect q q.Length))

    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen physicistQueueIntGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> PhysicistQueue.foldBack (fun (elem : int) (l' : int list)  -> elem::l') (q :?> PhysicistQueue<int>) [] = l |> classifyCollect q q.Length))
              
    fsCheck "PhysicistQueue OfSeq" (Prop.forAll (Arb.fromGen physicistQueueIntOfSeqGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> PhysicistQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> PhysicistQueue<int>) [] = l |> classifyCollect q q.Length))

    fsCheck "PhysicistQueue Snoc" (Prop.forAll (Arb.fromGen physicistQueueIntSnocGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> PhysicistQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> PhysicistQueue<int>) [] = l |> classifyCollect q q.Length))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``give None if there is no head in the queue``(eIQ : IQueue<obj>) =
    eIQ.TryGetHead |> should equal None

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("emptyIQueues")>]
let ``give None if there is no tail in the queue``(eIQ  : IQueue<obj>) =
    eIQ.TryGetTail |> should equal None

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart1")>]
let ``get the head from a queue``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q.Head = (List.nth l 0) |> classifyCollect q q.Length))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart1")>]
let ``get the head from a queue safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q.TryGetHead.Value = (List.nth l 0) |> classifyCollect q q.Length))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart2")>]
let ``get the tail from the queue``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((q : IQueue<int>), l) -> q.Tail.Head = (List.nth l 1) |> classifyCollect q q.Length))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart2")>]
let ``get the tail from a queue safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : IQueue<int>, l) -> q.TryGetTail.Value.Head = (List.nth l 1) |> classifyCollect q q.Length))
