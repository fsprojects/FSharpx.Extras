namespace FSharpx.Collections.Experimental.Tests

open FSharpx
open FSharpx.Collections.Experimental
open FSharpx.Collections.Experimental.Interfaces
open FSharpx.Tests.Properties
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit

module QueueGen =

    let snocThruList l q  =
        let rec loop (q' : 'a IQueue) (l' : 'a list) = 
            match l' with
            | hd :: [] -> q'.Snoc hd
            | hd :: tl -> loop (q'.Snoc hd) tl
            | [] -> q'
        
        loop q l

//BankersQueue
(*
IQueue generators from random ofSeq and/or snoc elements from random list 
*)
    let bankersQueueIntGen =
            gen { let! n = Gen.length1thru12
                  let! n2 = Gen.length1thru12
                  let! x = Gen.listInt n
                  let! y = Gen.listInt n2  
                  return ( (BankersQueue.ofSeq x |> snocThruList y), (x @ y) ) }

    let bankersQueueIntOfSeqGen =
            gen { let! n = Gen.length2thru12
                  let! n = Gen.length2thru12
                  let! x = Gen.listInt n
                  return ( (BankersQueue.ofSeq x) :> IQueue<int>, x) }

    let bankersQueueIntSnocGen =
            gen { let! n = Gen.length2thru12
                  let! x = Gen.listInt n
                  return ( (BankersQueue.empty() |> snocThruList x), x) }

    let bankersQueueObjGen =
            gen { let! n = Gen.length1thru12
                  let! n2 = Gen.length1thru12
                  let! x =  Gen.listObj n
                  let! y =  Gen.listObj n2  
                  return ( (BankersQueue.ofSeq x |> snocThruList y), (x @ y) ) }

    let bankersQueueStringGen =
            gen { let! n = Gen.length1thru12
                  let! n2 = Gen.length1thru12
                  let! x =  Gen.listString n
                  let! y =  Gen.listString n2  
                  return ( (BankersQueue.ofSeq x |> snocThruList y), (x @ y) ) }

//BatchedQueue
(*
non-IQueue generators from random ofList
*)
    let batchedQueueOfListGen =
            gen { let! n = Gen.length2thru12
                  let! x = Gen.listInt n
                  return ( (BatchedQueue.ofList x), x) }

(*
IQueue generators from random ofSeq and/or snoc elements from random list 
*)
    let batchedQueueIntGen =
            gen { let! n = Gen.length1thru12
                  let! n2 = Gen.length2thru12
                  let! x =  Gen.listInt n
                  let! y =  Gen.listInt n2
                  return ( (BatchedQueue.ofSeq x |> snocThruList y), (x @ y) ) }

    let batchedQueueIntOfSeqGen =
            gen { let! n = Gen.length1thru12
                  let! x = Gen.listInt n
                  return ( (BatchedQueue.ofSeq x) :> IQueue<int>, x) }

    let batchedQueueIntSnocGen =
            gen { let! n = Gen.length1thru12
                  let! x = Gen.listInt n
                  return ( (BatchedQueue.empty() |> snocThruList x), x) }

    let batchedQueueObjGen =
            gen { let! n = Gen.length2thru12
                  let! n2 = Gen.length1thru12
                  let! x =  Gen.listObj n
                  let! y =  Gen.listObj n2
                  return ( (BatchedQueue.ofSeq x |> snocThruList y), (x @ y) ) }

    let batchedQueueStringGen =
            gen { let! n = Gen.length1thru12
                  let! n2 = Gen.length2thru12
                  let! x =  Gen.listString n
                  let! y =  Gen.listString n2  
                  return ( (BatchedQueue.ofSeq x |> snocThruList y), (x @ y) ) }

                  //HoodMelvilleQueue
(*
non-IQueue generators from random ofList
*)
    let hoodMelvilleQueueOfListGen =
            gen { let! n = Gen.length2thru12
                  let! x = Gen.listInt n
                  return ( (HoodMelvilleQueue.ofList x), x) }

(*
IQueue generators from random ofSeq and/or snoc elements from random list 
*)
    let hoodMelvilleQueueIntGen =
            gen { let! n = Gen.length1thru12
                  let! n2 = Gen.length2thru12
                  let! x =  Gen.listInt n
                  let! y =  Gen.listInt n2
                  return ( (HoodMelvilleQueue.ofSeq x |> snocThruList y), (x @ y) ) }

    let hoodMelvilleQueueIntOfSeqGen =
            gen { let! n = Gen.length1thru12
                  let! x = Gen.listInt n
                  return ( (HoodMelvilleQueue.ofSeq x) :> IQueue<int>, x) }

    let hoodMelvilleQueueIntSnocGen =
            gen { let! n = Gen.length1thru12
                  let! x = Gen.listInt n
                  return ( (HoodMelvilleQueue.empty() |> snocThruList x), x) }

    let hoodMelvilleQueueObjGen =
            gen { let! n = Gen.length2thru12
                  let! n2 = Gen.length1thru12
                  let! x =  Gen.listObj n
                  let! y =  Gen.listObj n2
                  return ( (HoodMelvilleQueue.ofSeq x |> snocThruList y), (x @ y) ) }

    let hoodMelvilleQueueStringGen =
            gen { let! n = Gen.length1thru12
                  let! n2 = Gen.length2thru12
                  let! x =  Gen.listString n
                  let! y =  Gen.listString n2  
                  return ( (HoodMelvilleQueue.ofSeq x |> snocThruList y), (x @ y) ) }

//PhysicistQueue
(*
non-IQueue generators from random ofList
*)
    let physicistQueueOfListqGen =
            gen { let! n = Gen.length2thru12
                  let! x = Gen.listInt n
                  return ( (PhysicistQueue.ofList x), x) }

(*
IQueue generators from random ofSeq and/or snoc elements from random list 
*)
    let physicistQueueIntGen =
            gen { let! n = Gen.length1thru12
                  let! n2 = Gen.length2thru12
                  let! x = Gen.listInt n
                  let! y = Gen.listInt n2
                  return ( (PhysicistQueue.ofSeq x |> snocThruList y), (x @ y) ) }

    let physicistQueueIntOfSeqGen =
            gen { let! n = Gen.length2thru12
                  let! x = Gen.listInt n
                  return ( (PhysicistQueue.ofSeq x) :> IQueue<int>, x) }

    let physicistQueueIntSnocGen =
            gen { let! n = Gen.length2thru12
                  let! x = Gen.listInt n
                  return ( (PhysicistQueue.empty() |> snocThruList x), x) }

    let physicistQueueObjGen =
            gen { let! n = Gen.length2thru12
                  let! n2 = Gen.length1thru12
                  let! x =  Gen.listObj n
                  let! y =  Gen.listObj n2
                  return ( (PhysicistQueue.ofSeq x |> snocThruList y), (x @ y) ) }

    let physicistQueueStringGen =
            gen { let! n = Gen.length1thru12
                  let! n2 = Gen.length2thru12
                  let! x =  Gen.listString n
                  let! y =  Gen.listString n2  
                  return ( (PhysicistQueue.ofSeq x |> snocThruList y), (x @ y) ) }