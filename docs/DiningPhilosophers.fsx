﻿#r @"../bin/FSharpx.Extras.dll"

open System
open System.Threading
open FSharpx.Stm

let check p =
    match p with
    | true -> stm.Return(())
    | false -> retry ()

let forkIO (f : unit -> unit) = (new Thread(f)).Start()

let rec forever act : unit = act (); forever act

let take fork = 
    stm {
        let! ref_fork = readTVar (fork)
        do! check ref_fork
        do! writeTVar (fork) false
    } 

let adquire left_fork right_fork  =  
    stm { 
        do! take left_fork
        do! take right_fork
    } |> atomically    

let release left_fork right_fork = 
    stm { 
        do! writeTVar left_fork true
        do! writeTVar right_fork true
    } |> atomically    
    
let rng = new Random()

let n = 7 ;

let thinking = Array.zeroCreate n
let eating   = Array.zeroCreate n

let randomDelay () = 
    let waitTime = rng.Next(1000)
    Thread.Sleep waitTime
    uint64 waitTime

let eatOrThink i left_fork right_fork = 
    if (rng.Next(100)) > 50
    then 
        adquire left_fork right_fork
        printf "philosopher [%d] is eating.\n" i
        eating.[i] <- eating.[i] + randomDelay ()
        release left_fork right_fork
    else 
        printf "philosopher [%d] is thinking.\n" i
        thinking.[i] <- thinking.[i] + randomDelay ()

let philosofer i leftfork rightfork = 
    fun () -> forever (fun () -> eatOrThink i leftfork rightfork)

let timer = new System.Diagnostics.Stopwatch()

timer.Start()

let rec main () = 
    let forks = Array.init n (fun _ -> newTVar(true))
    for i in 0..n-1 do
        forkIO (philosofer i forks.[i] forks.[(i + 1) % n])
    
let onInterrupt _ _ =
    timer.Stop()
    printf "\ndone.\n"
    let total = float timer.ElapsedMilliseconds
    let p x = ((float x) * 100.0) / (float total)
    for i in 0..n-1 do
        printf "philosopher [%d] - percents: %.2f eating, %.2f thinking, %.2f obtaining forks\n"
            i (p eating.[i]) (p thinking.[i]) (100. - (p eating.[i] + p thinking.[i]))
    
Console.CancelKeyPress.AddHandler( new ConsoleCancelEventHandler( onInterrupt ))

main()