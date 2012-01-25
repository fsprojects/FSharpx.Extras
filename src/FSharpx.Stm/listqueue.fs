module FSharpx.Stm.ListQueue

open System
open System.Threading
open System.Collections.Generic
open FSharpx.Stm

type Node<'a> = Cons of 'a * TVar<Node<'a> > | Nil

type Queue<'a> = { head : TVar<Node<'a> >; last : TVar<Node<'a> > }

let new_queue () = { head = newTVar Nil; last = newTVar Nil }

let enqueue queue item =
    stm { let! oldLast = readTVar queue.last
          let  newLast = Cons (item, newTVar Nil)
          do! match oldLast with
              | Cons (_, next) -> writeTVar next newLast
              | Nil -> writeTVar queue.head newLast
          return! writeTVar queue.last newLast }

let ifM p x = if p then x else stm.Return(())

let dequeue queue =
    let is_nil node = match node with Nil -> true | _ -> false
    stm { let! oldHead = readTVar queue.head
          return! match oldHead with
                  | Cons (item, next) -> 
                      stm { let! newHead = readTVar next
                            do! writeTVar queue.head newHead
                            do! ifM (is_nil newHead) (writeTVar queue.last Nil)
                            return item }
                  | Nil -> retry () }

let liftM f x = stm { let! x' = x in return f x' }

let toList queue =
    let rec f node list =
        stm { let! node = readTVar node
              return! match node with
                      | Cons (item, next) -> f next (item :: list)
                      | Nil -> stm.Return(list) }
    f queue.head [] |> liftM List.rev

let fromList list =
    let f item (head, last) = 
        let newHead = Cons (item, newTVar head)
        newHead, match last with
                 | Nil -> newHead
                 | _ -> last
    let head, last = List.foldBack f list (Nil, Nil)
    { head = newTVar head; last = newTVar last }

let test l1 l2 num_threads =
    let q1 = fromList l1
    let q2 = fromList l2
    let move_item q1 q2 =
        stm { let! x = dequeue q1
              do! enqueue q2 x 
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
        stm { let! l1 = toList q1
              let! l2 = toList q2
              return l1,l2 } |> atomically
    terminate

let runTest () = 
    Console.WriteLine("Started.")
    let t = test [1..50] [51..100] 10 
    Thread.Sleep(3000)
    t () |> ignore
 