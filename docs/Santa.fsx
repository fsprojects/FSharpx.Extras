#r @"../bin/FSharpx.Extras.dll"

open System
open System.Threading
open FSharpx.Stm

let check p =
  match p with
  | true -> stm.Return(())
  | false -> retry ()

let forkIO (f : unit -> unit) = 
  let t = new Thread(f)
  t.Start()
  t

//----------------

//forever :: IO () -> IO ()
// Repeatedly perform the action
let rec forever act : unit = act (); forever act

//randomDelay :: IO ()
// Delay for a random time between 1 and 1000,000 microseconds
let rng = new Random()
let randomDelay () = 
  let waitTime = rng.Next(1000)
  Thread.Sleep(waitTime)

//choose :: [(STM a, a -> IO ())] -> IO ()
let choose choices : unit = 
  //stmActions :: [STM (IO ())]
  let stmActions = [ for (guard, rhs) in choices -> stm { let! v = guard in return (rhs v) } ]
  stmActions |> List.reduceBack orElse |> atomically

//---------------
type Gate = MkGate of int * TVar<int>

//newGate :: Int -> STM Gate
let newGate n = 
  stm { let tv = newTVar 0 in return MkGate (n, tv) }

//passGate :: Gate -> IO ()
let passGate (MkGate (n,tv)) =
  stm { let! n_left = readTVar tv
        do! check (n_left > 0)
        do! writeTVar tv (n_left-1)
        return () } |> atomically

//operateGate :: Gate -> IO ()
let operateGate (MkGate (n,tv)) =
   stm { return! writeTVar tv n } |> atomically
   stm { let! n_left = readTVar tv
         return! check (n_left = 0) } |> atomically

//---------------
type Group = MkGroup of int * TVar<int * Gate * Gate>

//newGroup :: Int -> IO Group
let newGroup n = 
  stm { let! g1 = newGate n
        let! g2 = newGate n
        let  tv = newTVar (n, g1, g2)
        return MkGroup (n, tv)  } |> atomically

//joinGroup :: Group -> IO (Gate,Gate)
let joinGroup (MkGroup (n,tv)) =
  stm { let! n_left, g1, g2 = readTVar tv
        do! check (n_left > 0) 
        do! writeTVar tv (n_left-1, g1, g2)
        return (g1,g2) } |> atomically

//awaitGroup :: Group -> STM (Gate,Gate)
let awaitGroup (MkGroup (n,tv)) =
  stm { let! n_left, g1, g2 = readTVar tv
        do! check (n_left = 0) 
        let! new_g1 = newGate n
        let! new_g2 = newGate n
        do! writeTVar tv (n,new_g1,new_g2)
        return (g1,g2) }

//---------------       
let rec main () = 
  let elf_gp = newGroup 3
  List.iter (fun n -> elf elf_gp n |> ignore) [1..10]
  let rein_gp = newGroup 9
  List.iter (fun n -> reindeer rein_gp n |> ignore) [1..9]
  forever (fun () -> santa elf_gp rein_gp)

and elf gp id = forkIO (fun () -> forever (fun () -> elf1 gp id; randomDelay ()))

and reindeer gp id = forkIO (fun () -> forever (fun () -> reindeer1 gp id; randomDelay ()))

//santa :: Group -> Group -> IO ()
and santa elf_group rein_group =
  Console.WriteLine("----------")
  choose [awaitGroup rein_group, run "deliver toys"; 
          awaitGroup elf_group,  run "meet in my study"]

//run :: String -> (Gate,Gate) -> IO ()
and run task (in_gate,out_gate) =
  Console.WriteLine("Ho! Ho! Ho! let's {0}", task)
  operateGate in_gate
  operateGate out_gate

//helper1 :: Group -> IO () -> IO ()
and helper1 group do_task =
  let in_gate, out_gate = joinGroup group
  passGate in_gate
  do_task ()
  passGate out_gate

//elf1, reindeer1 :: Group -> Int -> IO ()
and elf1 group id = helper1 group (fun () -> meetInStudy id)
and reindeer1 group id = helper1 group (fun () -> deliverToys id)

and meetInStudy id = Console.WriteLine("Elf {0} meeting in the study", id)
and deliverToys id = Console.WriteLine("Reindeer {0} delivering toys", id)

#if INTERACTIVE
main()
#else
[<EntryPoint>]
let entryPoint args = main(); 0
#endif
