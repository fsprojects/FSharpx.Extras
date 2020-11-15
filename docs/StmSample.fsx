#r @"../bin/FSharpx.Extras.dll"

open System
open System.Threading
open FSharpx.Stm

let test l1 l2 num_threads =
    let q1 = ListQueue.ofList l1
    let q2 = ListQueue.ofList l2
    let move_item q1 q2 =
        stm { let! x = ListQueue.dequeue q1
              do! ListQueue.enqueue q2 x 
              return x }
    let stop = newTVar false
    let rnd = new Random()
    let rec worker q1 q2 (fmt : string) =
        let x = 
            stm { let! stop' = readTVar stop
                  return! if not stop' 
                            then liftM Some (move_item q1 q2)
                            else stm.Return(None) } |> atomically
        match x with
        | Some x -> Console.WriteLine(fmt, Thread.CurrentThread.ManagedThreadId, x)
                    Thread.Sleep(rnd.Next(1000))
                    worker q1 q2 fmt
        | None -> ()
    let left_worker () = worker q1 q2 "Thread {0} moved item {1} left."
    let right_worker () = worker q2 q1 "Thread {0} moved item {1} right."
    let spawn (f : unit -> unit) = let t = new Thread(f) in t.Start(); t
    let threads = [ for _ in [1..num_threads] -> [spawn left_worker; spawn right_worker] ]
    let terminate () = 
        writeTVar stop true |> atomically
        threads |> Seq.concat |> Seq.iter (fun t -> t.Join()) 
        Console.WriteLine("Terminated.")
        stm { let! l1 = ListQueue.toList q1
              let! l2 = ListQueue.toList q2
              return l1,l2 } |> atomically
    terminate
    
let runTest () = 
    Console.WriteLine("Started.")
    let t = test [1..50] [51..100] 10 
    Thread.Sleep(3000)
    t () |> ignore
     
