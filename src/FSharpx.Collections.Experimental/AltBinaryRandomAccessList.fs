// originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/11/alternative-binary-random-access-list
//J.F. added remove, trys, length, append, rev

namespace FSharpx.Collections.Experimental

open System.Collections
open System.Collections.Generic

type AltBinRndAccList<'T> = 
    | Nil
    | Zero of AltBinRndAccList<'T * 'T>
    | One of 'T * AltBinRndAccList<'T * 'T>
    
    interface IRandomAccessList<'T> with

        member this.Cons (x : 'T) = AltBinRndAccList.cons x this :> _

        member this.Count() = AltBinRndAccList.length (0, 1, this)

        member this.Head =
            let x, _ = AltBinRndAccList.uncons this
            x

        member this.TryGetHead =
            match AltBinRndAccList.tryUncons this with
            | None -> None
            | Some( x, _) -> Some(x)

        member this.IsEmpty = AltBinRndAccList.isEmpty this

        member this.Length() = AltBinRndAccList.length (0, 1, this)

        member this.Lookup i = AltBinRndAccList.lookup i this

        member this.TryLookup i = AltBinRndAccList.tryLookup i this

        member this.Rev() = AltBinRndAccList.rev this :> _

        member this.Tail =
            let _, xs = AltBinRndAccList.uncons this
            xs :> _

        member this.TryGetTail =
            match AltBinRndAccList.tryUncons this with
            | None -> None
            | Some( _, Nil) -> None
            | Some( _, xs) -> Some(xs :> _)

        member this.Uncons =
            let x, xs = AltBinRndAccList.uncons this
            x, xs :> _

        member this.TryUncons =
            match AltBinRndAccList.tryUncons this with
            | None -> None
            | Some(x, xs) -> Some(x, xs :> _)

        member this.Update i y = AltBinRndAccList.fupdate ((fun x -> y), i, this) :> _
        
        member this.TryUpdate i y =
            match AltBinRndAccList.ftryUpdate ((fun x -> y), i, this) with
            | None -> None
            | Some(x) -> Some(x :> _)

        member this.GetEnumerator() = 
            let e = seq {
                match AltBinRndAccList.tryUncons this with
                | None -> () 
                | Some(x, xs) ->
                    yield x 
                    yield! xs}
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator
            
and AltBinRndAccList<'T> with

    static member internal cons (x : 'T) : AltBinRndAccList<'T> -> AltBinRndAccList<'T> = function
        | Nil ->  One (x, Nil) 
        | Zero ps -> One (x, ps) 
        | One(y, ps) ->  Zero(AltBinRndAccList.cons (x,y) ps) 
             
    static member internal isEmpty : AltBinRndAccList<'T> -> bool = function Nil -> true | _ -> false 

    static member internal length : int * int * AltBinRndAccList<'T> -> int = function
        | len, acc, Nil -> len
        | len, acc, One(x, Nil) -> len + acc
        | len, acc, One(x, ps) -> AltBinRndAccList.length ((len + acc), (2 * acc), ps)
        | len, acc, Zero ps -> AltBinRndAccList.length (len, (2 * acc), ps)

    static member internal lookup (i:int) : AltBinRndAccList<'T> -> 'T = function
        | Nil -> raise Exceptions.OutOfBounds
        | One(x, ps) ->
            if i = 0 then x else AltBinRndAccList.lookup (i-1) (Zero ps)
        | Zero ps ->
            let (x, y) = AltBinRndAccList.lookup (i/2) ps
            if i % 2 = 0 then x else y

    static member internal tryLookup (i:int) : AltBinRndAccList<'T> -> 'T option = function
        | Nil -> None
        | One(x, ps) ->
            if i = 0 then Some(x) else AltBinRndAccList.tryLookup (i-1) (Zero ps)
        | Zero ps ->
            match (AltBinRndAccList.tryLookup (i/2) ps) with
            | None -> None
            | Some (x, y) -> if i % 2 = 0 then Some(x) else Some(y)

    static member internal ofSeq (s:seq<'T>) : AltBinRndAccList<'T> = 
        if Seq.isEmpty s then Nil
        else
            let a = Array.ofSeq s
            let rec loop (acc: AltBinRndAccList<'T>) dec (a': array<'T>) =
                if dec < 0 then acc
                else loop (AltBinRndAccList.cons a'.[dec] acc) (dec - 1) a'
            loop Nil (a.Length - 1) a

    static member internal uncons : AltBinRndAccList<'T> -> 'T * AltBinRndAccList<'T> = function
        | Nil -> raise Exceptions.Empty
        | One(x, Nil) -> (x, Nil)
        | One(x, ps) -> (x, Zero ps)
        | Zero ps ->
              let (x,y), ps' = AltBinRndAccList.uncons ps
              x, (One (y, ps'))

    static member internal tryUncons : AltBinRndAccList<'T> -> ('T * AltBinRndAccList<'T>) option = function
        | Nil -> None
        | One(x, Nil) -> Some(x, Nil)
        | One(x, ps) -> Some(x, Zero ps)
        | Zero ps ->
              let (x,y), ps' = AltBinRndAccList.uncons ps
              Some(x, (One (y, ps')))

    static member internal fremove : int * array<'T> * int * AltBinRndAccList<'T> -> int * AltBinRndAccList<'T> = function
        | i, _, _, Nil -> raise Exceptions.OutOfBounds
        | 0, _, aIdx, One(x, ps) -> aIdx, Zero(ps)
        | i, a, aIdx, One (x, ps) -> 
            Array.set a aIdx x
            AltBinRndAccList.fremove (i-1, a, (aIdx+1), Zero ps)
        | 0, _, aIdx, Zero ps -> 
            let (x,y), ps' = AltBinRndAccList.uncons ps
            aIdx, One(y, ps')
        | 1, _, aIdx, Zero ps -> 
            let (x,y), ps' = AltBinRndAccList.uncons ps
            aIdx, One(x, ps')      
        | i, a, aIdx, Zero ps -> 
            let (x,y), ps' = AltBinRndAccList.uncons ps
            Array.set a aIdx x
            Array.set a (aIdx + 1) y
            AltBinRndAccList.fremove ((i-2), a, (aIdx + 2), Zero ps')

    static member internal ftryRemove : int * array<'T> * int * AltBinRndAccList<'T> -> int * AltBinRndAccList<'T> option = function
        | i, _, _, Nil -> 0, None
        | 0, _, aIdx, One(x, ps) -> aIdx, Some(Zero(ps))
        | i, a, aIdx, One (x, ps) -> 
            Array.set a aIdx x
            AltBinRndAccList.ftryRemove (i-1, a, (aIdx+1), Zero ps)
        | 0, _, aIdx, Zero ps -> 
            let (x,y), ps' = AltBinRndAccList.uncons ps
            aIdx, Some(One(y, ps'))
        | 1, _, aIdx, Zero ps -> 
            let (x,y), ps' = AltBinRndAccList.uncons ps
            aIdx, Some(One(x, ps'))
        | i, a, aIdx, Zero ps -> 
            let (x,y), ps' = AltBinRndAccList.uncons ps
            Array.set a aIdx x
            Array.set a (aIdx + 1) y
            AltBinRndAccList.ftryRemove ((i-2), a, (aIdx + 2), Zero ps')

    static member internal rev : AltBinRndAccList<'T> -> AltBinRndAccList<'T> = function
        | Nil -> Nil
        | xs -> 
            let rec loop xs' acc =
                match (AltBinRndAccList.tryUncons xs') with
                | None -> acc
                | Some(x, xs'') -> loop xs'' (AltBinRndAccList.cons x acc)
            loop xs Nil

    static member internal fupdate : ('T -> 'T) * int * AltBinRndAccList<'T> -> AltBinRndAccList<'T> = function
        | f, i, Nil -> raise Exceptions.OutOfBounds
        | f, 0, One(x, ps) -> One(f x, ps)
        | f, i, One (x, ps) -> AltBinRndAccList.cons x (AltBinRndAccList.fupdate (f, i-1, Zero ps))
        | f, i, Zero ps ->
            let f' (x, y) = if i % 2= 0 then f x, y else x, f y
            Zero(AltBinRndAccList.fupdate(f', i/2, ps))

    static member internal ftryUpdate : ('T -> 'T) * int * AltBinRndAccList<'T> -> AltBinRndAccList<'T> option = function
        | f, i, Nil -> None
        | f, 0, One(x, ps) -> Some(One(f x, ps))
        | f, i, One (x, ps) -> 
            match (AltBinRndAccList.ftryUpdate (f, i-1, Zero ps)) with
            | None -> None
            | Some(ps') -> Some(AltBinRndAccList.cons x ps')
        | f, i, Zero ps ->
            let f' (x, y) = if i % 2= 0 then f x, y else x, f y
            match (AltBinRndAccList.ftryUpdate(f', i/2, ps)) with
            | None -> None
            | Some(ps') -> Some(Zero(ps'))

    static member internal append : AltBinRndAccList<'T> * AltBinRndAccList<'T> -> AltBinRndAccList<'T> = function
        | Nil, Nil -> Nil
        | xs, Nil -> xs
        | Nil, ys -> ys
        | xs, ys -> 
            let xs' = AltBinRndAccList.rev xs
            let rec loop xs'' acc =
                match (AltBinRndAccList.tryUncons xs'') with
                | None -> acc
                | Some(x, xs'') -> loop xs'' (AltBinRndAccList.cons x acc)
            loop xs' ys

    ///O(log n). Returns a new random access list with the element added to the beginning.
    member this.Cons (x : 'T) = AltBinRndAccList.cons x this

    ///O(log n). Returns the first element.
    member this.Head =
        let x, _ = AltBinRndAccList.uncons this
        x

    ///O(log n). Returns option first element.
    member this.TryGetHead() =
        match AltBinRndAccList.tryUncons this with
        | None -> None
        | Some( x, _) -> Some(x)

    ///O(1). Returns true if the random access list has no elements.
    member this.IsEmpty = AltBinRndAccList.isEmpty this

    ///O(log n). Returns the count of elememts.
    member this.Length() = AltBinRndAccList.length (0, 1, this)

    ///O(log n). Returns element by index.
    member this.Lookup i = AltBinRndAccList.lookup i this

    ///O(log n). Returns option element by index.
    member this.TryLookup i = AltBinRndAccList.tryLookup i this

    ///O(n). Returns random access list with element removed by index.
    member this.Remove i = 
        if i = 0 then 
            match (AltBinRndAccList.uncons this) with
            | _, Zero(Nil) -> Nil
            | _, x -> x
        else
            let front = Array.create i (fst (AltBinRndAccList.uncons this))
            match (AltBinRndAccList.fremove (i, front, 0, this)) with
            | _, Zero(Nil) -> Nil
            | frontLen, x -> 
                let rec loop i' (front':'T array) (back:AltBinRndAccList<'T>) =
                    match i' with
                    
                    | i'' when i'' > -1 -> loop (i''- 1) front' ( AltBinRndAccList.cons front'.[i''] back)
                    | i'' -> back
                        
                loop (frontLen - 1) front x

    ///O(n). Returns option random access list with element removed by index.
    member this.TryRemove i = 
        if i = 0 then 
            match (AltBinRndAccList.uncons this) with
            | _, Zero(Nil) -> Some(Nil)
            | _, x -> Some(x)
        else
            let front = Array.create i (fst (AltBinRndAccList.uncons this))
            match (AltBinRndAccList.ftryRemove (i, front, 0, this)) with
            | _, None -> None
            | _, Some(Zero(Nil)) -> Some(Nil)
            | frontLen, Some(x) -> 
                let rec loop i' (front':'T array) (back:AltBinRndAccList<'T>) =
                    match i' with
                    
                    | i'' when i'' > -1 -> loop (i''- 1) front' ( AltBinRndAccList.cons front'.[i''] back)
                    | i'' -> Some(back)
                        
                loop (frontLen - 1) front x

    ///O(n). Returns random access list reversed.
    member this.Rev() = AltBinRndAccList.rev this 

    ///O(log n). Returns a new random access list of the elements trailing the first element.
    member this.Tail =
        let _, xs = AltBinRndAccList.uncons this
        xs 

    ///O(log n). Returns a option random access list of the elements trailing the first element.
    member this.TryGetTail =
        match AltBinRndAccList.tryUncons this with
        | None -> None
        | Some( _, Nil) -> None
        | Some( _, xs) -> Some(xs)

    ///O(log n). Returns the first element and tail.
    member this.Uncons =
        let x, xs = AltBinRndAccList.uncons this
        x, xs

    ///O(log n). Returns the option first element and tail.
    member this.TryUncons =
        match AltBinRndAccList.tryUncons this with
        | None -> None
        | Some(x, xs) -> Some(x, xs)

    ///O(log n). Returns random access list with element updated by index.
    member this.Update i y = AltBinRndAccList.fupdate ((fun x -> y), i, this)
        
    ///O(log n). Returns option random access list with element updated by index.
    member this.TryUpdate i y =
        match AltBinRndAccList.ftryUpdate ((fun x -> y), i, this) with
        | None -> None
        | Some(x) -> Some(x)
      
      
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
 module AltBinaryRandomAccessList = 
    //pattern discriminator

    let (|Cons|Nil|) (l: AltBinRndAccList<'T>) = match l.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    ///O(xs). Returns random access list from elements of 2 random access lists concatenated.
    let append xs ys = AltBinRndAccList.append (xs, ys)
   
    ///O(log n). Returns a new random access list with the element added to the beginning.
    let inline cons x (xs: AltBinRndAccList<'T>) = xs.Cons x   
  
    ///O(log n). Returns the first element.
    let head xs =
        let x, _ = AltBinRndAccList.uncons xs
        x

    ///O(1). Returns a empty random access list.
    let empty = Nil

    ///O(log n). Returns option first element.
    let tryGetHead xs =
        match (AltBinRndAccList.tryUncons xs) with
        | None -> None
        | Some( x, _) -> Some(x)

    ///O(1). Returns true if the random access list has no elements.
    let inline isEmpty (xs: AltBinRndAccList<'T>) = xs.IsEmpty

    ///O(log n). Returns the count of elememts.
    let inline length (xs: AltBinRndAccList<'T>) = xs.Length() 

    ///O(log n). Returns element by index.
    let rec lookup i xs = AltBinRndAccList.lookup i xs

    ///O(log n). Returns option element by index.
    let rec tryLookup i xs = AltBinRndAccList.tryLookup i xs

    ///O(n). Returns random access list from the sequence.
    let ofSeq s = AltBinRndAccList.ofSeq s

    ///O(n). Returns random access list with element removed by index.
    let inline remove i (xs: AltBinRndAccList<'T>) = xs.Remove i 

    ///O(n). Returns option random access list with element removed by index.
    let inline tryRemove i (xs: AltBinRndAccList<'T>) = xs.TryRemove i

    ///O(n). Returns random access list reversed.
    let inline rev (xs: AltBinRndAccList<'T>) = xs.Rev()

    ///O(log n). Returns a new random access list of the elements trailing the first element.
    let tail xs =
        let _, xs' = AltBinRndAccList.uncons xs
        xs'

    ///O(log n). Returns a option random access list of the elements trailing the first element.
    let tryGetTail xs =
        match (AltBinRndAccList.tryUncons xs) with
        | None -> None
        | Some( _, xs') -> Some(xs')

    ///O(log n). Returns the first element and tail.
    let inline uncons (xs: AltBinRndAccList<'T>) = xs.Uncons

    ///O(log n). Returns the option first element and tail.
    let inline tryUncons (xs: AltBinRndAccList<'T>) = xs.TryUncons

    ///O(log n). Returns random access list with element updated by index.
    let inline update i y (xs: AltBinRndAccList<'T>) = xs.Update i y

    ///O(log n). Returns option random access list with element updated by index.
    let inline tryUpdate i y (xs: AltBinRndAccList<'T>) = xs.TryUpdate i y