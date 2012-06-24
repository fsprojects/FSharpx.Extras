
open FSharpx.TimeMeasurement
open FSharpx.DataStructures.Vector
open FSharpx.DataStructures

open System
let c = 5
let r = new System.Random()

let getResultAndTime run =
    System.GC.Collect(9)
    System.GC.Collect(9)
    let start = DateTime.Now
    let r = run()
    (r, (DateTime.Now - start).TotalMilliseconds)

// **************************************************************************************************************
let runSomeTest() =
    let reps = 1000000
    let fromRep x = (x % 6713, x)
    let times =
        [
         ("TrieMap", snd (getResultAndTime (fun () -> Seq.init reps fromRep |> TrieMap.ofSeq)))
         ("Packed", snd (getResultAndTime (fun () -> Seq.init reps fromRep |> Experimental.TrieMap_Packed.ofSeq)))
         
         ("TrieMap", snd (getResultAndTime (fun () -> Seq.init reps fromRep |> TrieMap.ofSeq)))
         ("Packed", snd (getResultAndTime (fun () -> Seq.init reps fromRep |> Experimental.TrieMap_Packed.ofSeq)))
         ("TrieMap", snd (getResultAndTime (fun () -> Seq.init reps fromRep |> TrieMap.ofSeq)))
         ("Packed", snd (getResultAndTime (fun () -> Seq.init reps fromRep |> Experimental.TrieMap_Packed.ofSeq)))
         
         ]

         (*
    System.GC.Collect(9)
    let (map, mapTime) = getResultAndTime (fun () -> Seq.init reps (fun x -> (x, x)) |> TrieMap.ofSeq)
    System.GC.Collect(9)
    let (map, mapTime_secondary) = getResultAndTime (fun () -> Seq.init reps (fun x -> (x, x)) |> TrieMap.ofSeq)
    System.GC.Collect(9)
    let (map, mapTime_tertiary) = getResultAndTime (fun () -> Seq.init reps (fun x -> (x, x)) |> TrieMap.ofSeq)
    System.GC.Collect(9)
    let (map2, map2time) = getResultAndTime (fun () -> Seq.init reps (fun x -> (x, x)) |> Experimental.TrieMap_Packed.ofSeq)
    let something = map.[533]
    Console.WriteLine(mapTime.ToString() + " " + mapTime_secondary.ToString() + " " + mapTime_tertiary.ToString() + " " + map2time.ToString())
    *)
    times |> List.iter (fun x -> Console.WriteLine(x.ToString()))
    ()
runSomeTest()
// **************************************************************************************************************

let array n = [|for i in 1..n -> r.Next()|]

let vector n = 
    let v = ref empty
    for i in 1..n do
        v := cons (r.Next()) !v
    !v

let initArrayAndVector n =
    printfn "Init with n = %d" n

    compareTwoRuntimes c 
        "  Array" (fun () -> array n)
        "  PersistentVector" (fun () -> vector n)

let initArrayAndVectorFromList n =
    printfn "Init from seq with n = %d" n
    let list = [ for i in 1..n -> r.Next() ]

    compareTwoRuntimes c
        "  Array" (fun () -> Array.ofSeq list)
        "  PersistentVector" (fun () -> ofSeq list)

let lookupInArrayAndVector n count =
    printfn "%d Lookups in size n = %d" count n
    let array = array n
    let vector = vector n

    compareTwoRuntimes c
        "  Array" (fun () -> for i in 1..count do array.[r.Next n] |> ignore)
        "  PersistentVector" (fun () -> for i in 1..count do nth (r.Next n) vector |> ignore)


let replaceInArrayAndVector n count =
    printfn "%d writes in size n = %d" count n
    let array = array n
    let vector = vector n

    compareTwoRuntimes c
        "  Array" (fun () -> for i in 1..count do array.[r.Next n] <- r.Next())
        "  PersistentVector" (fun () -> for i in 1..count do assocN (r.Next n) (r.Next()) vector |> ignore)

initArrayAndVector 10000
initArrayAndVector 100000
initArrayAndVector 1000000

initArrayAndVectorFromList 10000
initArrayAndVectorFromList 100000
initArrayAndVectorFromList 1000000

lookupInArrayAndVector 10000 10000
lookupInArrayAndVector 100000 10000
lookupInArrayAndVector 1000000 10000
lookupInArrayAndVector 10000000 10000

replaceInArrayAndVector 10000 10000
replaceInArrayAndVector 100000 10000
replaceInArrayAndVector 1000000 10000
replaceInArrayAndVector 10000000 10000

printfn "Ready."
System.Console.ReadKey() |> ignore