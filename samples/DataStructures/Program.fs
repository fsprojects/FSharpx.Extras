let c = 5
let r = new System.Random()

open FSharpx.TimeMeasurement
open FSharpx.DataStructures.PersistentVector

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