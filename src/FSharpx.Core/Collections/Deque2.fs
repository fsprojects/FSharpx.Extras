//inspired by, and init and tail algorithms from, BatchedDeque http://fssnip.net/dJ

namespace FSharpx.Collections

open System.Collections
open System.Collections.Generic

type Deque<'T> (front, rBack) = 
    let hashCode = ref None
    member internal this.front = front
    member internal this.rBack = rBack

    override this.GetHashCode() =
            match !hashCode with
            | None ->
                let mutable hash = 1
                for x in this do
                    hash <- 31 * hash + Unchecked.hash x
                hashCode := Some hash
                hash
            | Some hash -> hash

    override this.Equals(other) =
        match other with
        | :? Deque<'T> as y -> 
            if this.Length <> y.Length then false 
            else
                if this.GetHashCode() <> y.GetHashCode() then false
                else Seq.forall2 (Unchecked.equals) this y
        | _ -> false

    member this.Conj x = Deque(front, x::rBack)

    member this.Cons x =  Deque(x::front, rBack)
           
    member this.Head =
        match front, rBack with
        | [], [] -> raise (new System.Exception("Deque is empty"))
        | hd::tl, _ -> hd
        | [], xs -> List.rev xs |> List.head

    member this.TryHead : 'T option =
        match front, rBack with
        | [], [] -> None
        | hd::tl, _ -> Some(hd)
        | [], xs -> 
            let x = List.rev xs |> List.head 
            Some(x)

    member this.Initial = 
        match front, rBack with 
        | [],  [] -> raise (new System.Exception("Deque is empty"))
        | _ , x::xs -> Deque(front, xs)
        | _ ,   [] ->       //splits front in two, favoring frontbot for odd length
            let half = front.Length / 2
            let a = Array.ofList front
            let frontA = Array.create half front.Head
            let rBackA = Array.create ((front.Length - half) - 1) front.Head
            Array.blit a 0 frontA 0 frontA.Length
            Array.blit a frontA.Length rBackA 0 rBackA.Length
            let rBackA' = Array.rev rBackA
            Deque(List.ofArray frontA, List.ofArray rBackA')
    
    member this.TryInitial = 
        match front, rBack with 
        | [],  [] -> None
        | _ , x::xs -> Some(Deque(front, xs))
        | _ ,   [] ->       //splits front in two, favoring frontbot for odd length
            let half = front.Length / 2
            let a = Array.ofList front
            let frontA = Array.create half front.Head
            let rBackA = Array.create ((front.Length - half) - 1) front.Head
            Array.blit a 0 frontA 0 frontA.Length
            Array.blit a frontA.Length rBackA 0 rBackA.Length
            let rBackA' = Array.rev rBackA
            Some(Deque(List.ofArray frontA, List.ofArray rBackA')) 
          
    member this.IsEmpty =  
        match front, rBack with
        | [], [] -> true | _ -> false

    member this.Last = 
        match front, rBack with
        | [], [] -> raise (new System.Exception("Deque is empty"))
        | xs, [] -> List.rev xs |> List.head
        | _, hd::tl -> hd

    member this.TryLast = 
        match front, rBack with
        | [], [] -> None
        | xs, [] -> Some(List.rev xs |> List.head)
        | _, hd::tl -> Some(hd)

    member this.Length = front.Length + rBack.Length

    member this.Rev = 
        (new Deque<'T>(rBack, front))

    member this.Tail =
        match front, rBack with
        | [],  [] -> raise (new System.Exception("Deque is empty"))
        | x::xs,  _ ->  Deque(xs, rBack)
        | _,  _ ->      //splits rear in two, favoring rearbot for odd length
            let half = rBack.Length / 2
            let a = Array.ofList rBack
            let frontA = Array.create half rBack.Head
            let rBackA = Array.create ((rBack.Length - half) - 1) rBack.Head
            Array.blit a 0 rBackA 0 rBackA.Length
            Array.blit a rBackA.Length frontA 0 frontA.Length
            let frontA' = Array.rev frontA
            Deque(List.ofArray frontA', List.ofArray rBackA)

    member this.TryTail =
        match front, rBack with
        | [],  [] -> None
        | x::xs,  _ ->  Some(Deque(xs, rBack))
        | _,  _ ->      //splits rear in two, favoring rearbot for odd length
            let half = rBack.Length / 2
            let a = Array.ofList rBack
            let frontA = Array.create half rBack.Head
            let rBackA = Array.create ((rBack.Length - half) - 1) rBack.Head
            Array.blit a 0 rBackA 0 rBackA.Length
            Array.blit a rBackA.Length frontA 0 frontA.Length
            let frontA' = Array.rev frontA
            Some(Deque(List.ofArray frontA', List.ofArray rBackA))

    member this.Uncons =  
        match front, rBack with
        | [], [] -> raise (new System.Exception("Deque is empty"))
        | _, _ -> this.Head, this.Tail

    member this.TryUncons =  
        match front, rBack with
        | [], [] -> None
        | _, _ -> Some(this.Head, this.Tail)

    member this.Unconj =  
        match front, rBack with
        | [], [] -> raise (new System.Exception("Deque is empty"))
        | _, _ -> this.Initial, this.Last

    member this.TryUnconj =  
        match front, rBack with
        | [], [] -> None
        | _, _ -> Some(this.Initial, this.Last)
          
    with

    interface IEnumerable<'T> with

        member this.GetEnumerator() = 
            let e = seq {
                  yield! front
                  yield! (List.rev rBack)}
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Deque =

    let (|Cons|Nil|) (q : Deque<'T>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    let (|Conj|Nil|) (q : Deque<'T>) = match q.TryUnconj with Some(a,b) -> Conj(a,b) | None -> Nil

    let inline conj (x : 'T) (q : Deque<'T>) = (q.Conj x) 

    let inline cons (x : 'T) (q : Deque<'T>) = q.Cons x 

    let empty<'T> = Deque<'T>(List.Empty, List.Empty)

    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : Deque<'T>) = 
        let s = List.fold f state q.front
        List.fold f s (List.rev q.rBack)

    let foldBack (f : ('T -> 'State -> 'State)) (q : Deque<'T>) (state : 'State) =  
        let s = List.foldBack f (List.rev q.rBack) state 
        (List.foldBack f q.front s)

    let inline head (q : Deque<'T>) = q.Head

    let inline tryHead (q : Deque<'T>) = q.TryHead

    let inline initial (q : Deque<'T>) = q.Initial 

    let inline tryInitial (q : Deque<'T>) = q.TryInitial 

    let inline isEmpty (q : Deque<'T>) = q.IsEmpty

    let inline last (q : Deque<'T>) = q.Last

    let inline tryLast (q : Deque<'T>) = q.TryLast

    let inline length (q : Deque<'T>) = q.Length

    let ofCatLists (xs : 'T list) (ys : 'T list) = Deque<'T>(xs, (List.rev ys))

    let ofList (xs : 'T list) = Deque<'T>(xs, [])

    let ofSeq (xs:seq<'T>) = Deque<'T>((List.ofSeq xs), [])

    let inline rev (q : Deque<'T>) = q.Rev

    let singleton (x : 'T) = Deque<'T>([x], List.Empty)

    let inline tail (q : Deque<'T>) = q.Tail 

    let inline tryTail (q : Deque<'T>) = q.TryTail 

    let inline uncons (q : Deque<'T>) = q.Uncons

    let inline tryUncons (q : Deque<'T>) = q.TryUncons

    let inline unconj (q : Deque<'T>) = q.Unconj

    let inline toSeq (q : Deque<'T>) = q :> seq<'T>

    let inline tryUnconj (q : Deque<'T>) = q.TryUnconj