module FSharpx.Stm.ArrayQueue

open System
open System.Threading
open System.Collections.Generic

type Queue<'a> = { head : TVar<int>; used : TVar<int>; len : int; a : TVar<'a>[] }

let newQueueClass n = 
  { head = newTVar 0;
    used = newTVar 0;
    len = n;
    a = Array.zeroCreate n |> Array.map newTVar }

let newQueueStruct n = 
  { head = newTVar 0;
    used = newTVar 0;
    len = n
    a = Array.zeroCreate n |> Array.map newTVar }

let enqueue queue item =
  stm { let! used = readTVar queue.used
        return! if used < queue.len
                then stm { let! head = readTVar queue.head
                           do! writeTVar queue.a.[(head+used) % queue.len] item
                           return! writeTVar queue.used (used+1) }
                else retry () }

let dequeue queue =
  stm { let! used = readTVar queue.used
        return! if used > 0
                then stm { let! head = readTVar queue.head
                           let! item = readTVar queue.a.[head]
                           do! writeTVar queue.head ((head+1) % queue.len)
                           return! writeTVar queue.used (used-1) }
                else retry () }

let toList queue =
  stm { let! used = readTVar queue.used
        let! head = readTVar queue.head
        return! Seq.init used (fun i -> queue.a.[(head+i) % queue.len]) 
                |> mapM readTVar 
                |> liftM Seq.toList }

let fromList n list =
  let l = list |> List.toArray
  let a = Array.zeroCreate n  
  Array.blit l 0 a 0 l.Length
  { head = newTVar 0;
    used = newTVar l.Length;
    len = n
    a = a |> Array.map newTVar }  
