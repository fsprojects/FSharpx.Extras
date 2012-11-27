module FSharpx.DataStructures.Tests.IQueueTest

open FSharpx
open FSharpx.DataStructures
open FSharpx.DataStructures.Interfaces
open FSharpx.Tests.Properties
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

let emptyIQueues =
    let v = Array.create 4 (BankersQueue.empty() :> IQueue<obj>)
    v.[1] <- (BatchedQueue.empty() :> IQueue<obj>)
    v.[2] <- (PhysicistQueue.empty() :> IQueue<obj>)
    v.[3] <- (HoodMelvilleQueue.empty() :> IQueue<obj>)
    v

// NUnit TestCaseSource does not understand array of tuples at runtime
let intGens start =
    let v = Array.create 12 (box (QueueGen.bankersQueueIntGen, "BankersQueue"))
    v.[1] <- box ((QueueGen.bankersQueueIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "BankersQueue OfSeq")
    v.[2] <- box ((QueueGen.bankersQueueIntSnocGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "BankersQueue Snoc")
    v.[3] <- box (QueueGen.batchedQueueIntGen, "BatchedQueue")
    v.[4] <- box ((QueueGen.batchedQueueIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "BatchedQueue OfSeq")
    v.[5] <- box ((QueueGen.batchedQueueIntSnocGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "BatchedQueue Snoc")
    v.[6] <- box (QueueGen.hoodMelvilleQueueIntGen, "HoodMelvilleQueue")
    v.[7] <- box ((QueueGen.hoodMelvilleQueueIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "HoodMelvilleQueue OfSeq")
    v.[8] <- box ((QueueGen.hoodMelvilleQueueIntSnocGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "HoodMelvilleQueue Snoc")
    v.[9] <- box (QueueGen.physicistQueueIntGen, "PhysicistQueue")
    v.[10] <- box ((QueueGen.physicistQueueIntOfSeqGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "PhysicistQueue OfSeq")
    v.[11] <- box ((QueueGen.physicistQueueIntSnocGen |> Gen.suchThat (fun (q, l) -> l.Length >= start)), "PhysicistQueue Snoc")
    v

let objGens =
    let v = Array.create 4 (box (QueueGen.bankersQueueObjGen, "BankersQueue"))
    v.[1] <- box (QueueGen.batchedQueueObjGen, "BatchedQueue")
    v.[2] <- box (QueueGen.hoodMelvilleQueueObjGen, "HoodMelvilleQueue")
    v.[3] <- box (QueueGen.physicistQueueObjGen, "PhysicistQueue")
    v

let stringGens =
    let v = Array.create 4 (box (QueueGen.bankersQueueStringGen, "BankersQueue"))
    v.[1] <- box (QueueGen.batchedQueueStringGen, "BatchedQueue")
    v.[2] <- box (QueueGen.hoodMelvilleQueueStringGen, "HoodMelvilleQueue")
    v.[3] <- box (QueueGen.physicistQueueStringGen, "PhysicistQueue")
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
let ``cons pattern discriminator - HoodMelvilleQueue``() =
    let q = HoodMelvilleQueue.ofSeq  ["f";"e";"d";"c";"b";"a"]
    
    let h1, t1 = 
        match q with
        | HoodMelvilleQueue.Cons(h, t) -> h, t
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
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``fold matches build list rev``() =

    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen QueueGen.batchedQueueIntGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> BatchedQueue<int> |> BatchedQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length())))
              
    fsCheck "BatchedQueue OfSeq" (Prop.forAll (Arb.fromGen QueueGen.batchedQueueIntOfSeqGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> BatchedQueue<int> |> BatchedQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length())))

    fsCheck "BatchedQueue Snoc" (Prop.forAll (Arb.fromGen QueueGen.batchedQueueIntSnocGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> BatchedQueue<int> |> BatchedQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length())))

    fsCheck "HoodMelvilleQueue" (Prop.forAll (Arb.fromGen QueueGen.hoodMelvilleQueueIntGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> HoodMelvilleQueue<int> |> HoodMelvilleQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length())))
              
    fsCheck "HoodMelvilleQueue OfSeq" (Prop.forAll (Arb.fromGen QueueGen.hoodMelvilleQueueIntOfSeqGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> HoodMelvilleQueue<int> |> HoodMelvilleQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length())))

    fsCheck "HoodMelvilleQueue Snoc" (Prop.forAll (Arb.fromGen QueueGen.hoodMelvilleQueueIntSnocGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> HoodMelvilleQueue<int> |> HoodMelvilleQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length())))

    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen QueueGen.physicistQueueIntGen) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> PhysicistQueue<int> |> PhysicistQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length())))
              
    fsCheck "PhysicistQueue OfSeq" (Prop.forAll (Arb.fromGen (QueueGen.physicistQueueIntOfSeqGen)) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> PhysicistQueue<int> |> PhysicistQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length())))

    fsCheck "PhysicistQueue Snoc" (Prop.forAll (Arb.fromGen (QueueGen.physicistQueueIntSnocGen)) 
        (fun ((q :IQueue<int>), (l : int list)) -> q :?> PhysicistQueue<int> |> PhysicistQueue.fold (fun (l' : int list) (elem : int) -> elem::l') [] = (List.rev l) |> classifyCollect q (q.Length())))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``foldback matches build list``() =

    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen QueueGen.batchedQueueIntGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> BatchedQueue.foldBack (fun (elem : int) (l' : int list)  -> elem::l') (q :?> BatchedQueue<int>) [] = l |> classifyCollect q (q.Length())))
              
    fsCheck "BatchedQueue OfSeq" (Prop.forAll (Arb.fromGen QueueGen.batchedQueueIntOfSeqGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> BatchedQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> BatchedQueue<int>) [] = l |> classifyCollect q (q.Length())))

    fsCheck "BatchedQueue Snoc" (Prop.forAll (Arb.fromGen QueueGen.batchedQueueIntSnocGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> BatchedQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> BatchedQueue<int>) [] = l |> classifyCollect q (q.Length())))

    fsCheck "HoodMelvilleQueue" (Prop.forAll (Arb.fromGen QueueGen.hoodMelvilleQueueIntGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> HoodMelvilleQueue.foldBack (fun (elem : int) (l' : int list)  -> elem::l') (q :?> HoodMelvilleQueue<int>) [] = l |> classifyCollect q (q.Length())))
              
    fsCheck "HoodMelvilleQueue OfSeq" (Prop.forAll (Arb.fromGen QueueGen.hoodMelvilleQueueIntOfSeqGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> HoodMelvilleQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> HoodMelvilleQueue<int>) [] = l |> classifyCollect q (q.Length())))

    fsCheck "HoodMelvilleQueue Snoc" (Prop.forAll (Arb.fromGen QueueGen.hoodMelvilleQueueIntSnocGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> HoodMelvilleQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> HoodMelvilleQueue<int>) [] = l |> classifyCollect q (q.Length())))

    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen QueueGen.physicistQueueIntGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> PhysicistQueue.foldBack (fun (elem : int) (l' : int list)  -> elem::l') (q :?> PhysicistQueue<int>) [] = l |> classifyCollect q (q.Length())))
              
    fsCheck "PhysicistQueue OfSeq" (Prop.forAll (Arb.fromGen QueueGen.physicistQueueIntOfSeqGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> PhysicistQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> PhysicistQueue<int>) [] = l |> classifyCollect q (q.Length())))

    fsCheck "PhysicistQueue Snoc" (Prop.forAll (Arb.fromGen QueueGen.physicistQueueIntSnocGen) 
        (fun ((q : IQueue<int>), (l : int list)) -> PhysicistQueue.foldBack (fun (elem : int) (l' : int list) -> elem::l') (q :?> PhysicistQueue<int>) [] = l |> classifyCollect q (q.Length())))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart1")>]
let ``get head from queue``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q.Head = (List.nth l 0) |> classifyCollect q (q.Length())))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart1")>]
let ``get head from queue safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q.TryGetHead.Value = (List.nth l 0) |> classifyCollect q (q.Length())))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart2")>]
let ``get tail from queue``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun ((q : IQueue<int>), l) -> q.Tail.Head = (List.nth l 1) |> classifyCollect q (q.Length())))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("intGensStart2")>]
let ``get tail from queue safely``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q : IQueue<int>, l) -> q.TryGetTail.Value.Head = (List.nth l 1) |> classifyCollect q (q.Length())))

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
let ``int queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<int>, l) -> q |> Seq.toList = l |> classifyCollect q (q.Length())))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("objGens")>]
let ``obj queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<obj>, l) -> q |> Seq.toList = l |> classifyCollect q (q.Length())))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
[<TestCaseSource("stringGens")>]
let ``string queue builds and serializes``(x : obj) =
    let genAndName = unbox x 
    fsCheck (snd genAndName) (Prop.forAll (Arb.fromGen (fst genAndName)) (fun (q :IQueue<string>, l) -> q |> Seq.toList = l |> classifyCollect q (q.Length())))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``reverse . reverse = id``() =

    fsCheck "BankersQueue" (Prop.forAll (Arb.fromGen QueueGen.bankersQueueObjGen) 
        (fun (q, l) -> q :?> BankersQueue<obj> |> BankersQueue.rev |> BankersQueue.rev |> Seq.toList = (q |> Seq.toList) |> classifyCollect q (q.Length())))
    
    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen QueueGen.batchedQueueObjGen) 
        (fun (q, l) -> q :?> BatchedQueue<obj> |> BatchedQueue.rev |> BatchedQueue.rev |> Seq.toList = (q |> Seq.toList) |> classifyCollect q (q.Length())))
    
    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen QueueGen.physicistQueueIntGen) 
        (fun (q, l) -> q :?> PhysicistQueue<int> |> PhysicistQueue.rev |> PhysicistQueue.rev |> Seq.toList = (q |> Seq.toList) |> classifyCollect q (q.Length())))

[<Test>]
[<Category("nonIQueue")>]
[<Property("Category", "nonIQueue")>]
let ``ofList build and serialize``() =

    fsCheck "BatchedQueue" (Prop.forAll (Arb.fromGen QueueGen.batchedQueueOfListGen) 
        (fun ((q : BatchedQueue<int>), (l : int list)) -> q |> Seq.toList = l |> classifyCollect q q.Length))

    fsCheck "HoodMelvilleQueue" (Prop.forAll (Arb.fromGen QueueGen.hoodMelvilleQueueOfListGen) 
        (fun ((q : HoodMelvilleQueue<int>), (l : int list)) -> q |> Seq.toList = l |> classifyCollect q q.Length))

    fsCheck "PhysicistQueue" (Prop.forAll (Arb.fromGen QueueGen.physicistQueueOfListqGen) 
        (fun ((q : PhysicistQueue<int>), (l : int list)) -> q |>  Seq.toList = l |> classifyCollect q q.Length))

[<Test>]
[<Category("IQueue")>]
[<Property("Category", "IQueue")>]
let ``TryUncons wind-down to None``() =
    let qBn = BankersQueue.ofSeq  ["f";"e";"d";"c";"b";"a"] :> IQueue<string>
    let qBt = BatchedQueue.ofSeq  ["f";"e";"d";"c";"b";"a"] :> IQueue<string>
    let qH = HoodMelvilleQueue.ofSeq  ["f";"e";"d";"c";"b";"a"] :> IQueue<string>
    let qP = PhysicistQueue.ofSeq  ["f";"e";"d";"c";"b";"a"] :> IQueue<string>

    let rec loop (iq : IQueue<string>) = 
        match (iq.TryUncons) with
        | Some(hd, tl) ->  loop tl
        | None -> ()

    loop qBn
    loop qBt
    loop qH
    loop qP

    true |> should equal true