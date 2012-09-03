// originally published by Julien
// original implementation taken from http://lepensemoi.free.fr/index.php/2010/02/11/alternative-binary-random-access-list
//J.F. added remove, trys, length, append, rev


module FSharpx.DataStructures.AltBinaryRandomAccessList

type AltBinRndAccList<'a> =
    | Nil
    | Zero of AltBinRndAccList<'a * 'a>
    | One of 'a * AltBinRndAccList<'a * 'a>
    //polymorphic recursion cannot be achieved through let-bound functions
    //hence we use static member methods
    static member cons (x : 'a) : AltBinRndAccList<'a> -> AltBinRndAccList<'a> = function
        | Nil -> One (x, Nil)
        | Zero ps -> One (x, ps)
        | One(y, ps) ->  Zero(AltBinRndAccList.cons (x,y) ps)

    static member uncons : AltBinRndAccList<'a> -> 'a * AltBinRndAccList<'a> = function
        | Nil -> raise Exceptions.Empty
        | One(x, Nil) -> (x, Nil)
        | One(x, ps) -> (x, Zero ps)
        | Zero ps ->
              let (x,y), ps' = AltBinRndAccList.uncons ps
              x, (One (y, ps'))

    static member tryUncons : AltBinRndAccList<'a> -> ('a * AltBinRndAccList<'a>) option = function
        | Nil -> None
        | One(x, Nil) -> Some(x, Nil)
        | One(x, ps) -> Some(x, Zero ps)
        | Zero ps ->
              let (x,y), ps' = AltBinRndAccList.uncons ps
              Some(x, (One (y, ps')))

    static member lookup (i:int) : AltBinRndAccList<'a> -> 'a = function
        | Nil -> raise Exceptions.OutOfBounds
        | One(x, ps) ->
            if i = 0 then x else AltBinRndAccList.lookup (i-1) (Zero ps)
        | Zero ps ->
            let (x, y) = AltBinRndAccList.lookup (i/2) ps
            if i % 2 = 0 then x else y

    static member tryLookup (i:int) : AltBinRndAccList<'a> -> 'a option = function
        | Nil -> None
        | One(x, ps) ->
            if i = 0 then Some(x) else AltBinRndAccList.tryLookup (i-1) (Zero ps)
        | Zero ps ->
            match (AltBinRndAccList.tryLookup (i/2) ps) with
            | None -> None
            | Some (x, y) -> if i % 2 = 0 then Some(x) else Some(y)

    static member fupdate : ('a -> 'a) * int * AltBinRndAccList<'a> -> AltBinRndAccList<'a> = function
        | f, i, Nil -> raise Exceptions.OutOfBounds
        | f, 0, One(x, ps) -> One(f x, ps)
        | f, i, One (x, ps) -> AltBinRndAccList.cons x (AltBinRndAccList.fupdate (f, i-1, Zero ps))
        | f, i, Zero ps ->
            let f' (x, y) = if i % 2= 0 then f x, y else x, f y
            Zero(AltBinRndAccList.fupdate(f', i/2, ps))

    static member tryfUpdate : ('a -> 'a) * int * AltBinRndAccList<'a> -> AltBinRndAccList<'a> option = function
        | f, i, Nil -> None
        | f, 0, One(x, ps) -> Some(One(f x, ps))
        | f, i, One (x, ps) -> 
            match (AltBinRndAccList.tryfUpdate (f, i-1, Zero ps)) with
            | None -> None
            | Some(ps') -> Some(AltBinRndAccList.cons x ps')
        | f, i, Zero ps ->
            let f' (x, y) = if i % 2= 0 then f x, y else x, f y
            match (AltBinRndAccList.tryfUpdate(f', i/2, ps)) with
            | None -> None
            | Some(ps') -> Some(Zero(ps'))

    static member internal remove : int * array<'a> * int * AltBinRndAccList<'a> -> int * AltBinRndAccList<'a> = function
        | i, _, _, Nil -> raise Exceptions.OutOfBounds
        | 0, _, aIdx, One(x, ps) -> aIdx, Zero(ps)
        | i, a, aIdx, One (x, ps) -> 
            Array.set a aIdx x
            AltBinRndAccList.remove (i-1, a, (aIdx+1), Zero ps)
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
            AltBinRndAccList.remove ((i-2), a, (aIdx + 2), Zero ps')

    static member internal tryRemove : int * array<'a> * int * AltBinRndAccList<'a> -> int * AltBinRndAccList<'a> option = function
        | i, _, _, Nil -> 0, None
        | 0, _, aIdx, One(x, ps) -> aIdx, Some(Zero(ps))
        | i, a, aIdx, One (x, ps) -> 
            Array.set a aIdx x
            AltBinRndAccList.tryRemove (i-1, a, (aIdx+1), Zero ps)
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
            AltBinRndAccList.tryRemove ((i-2), a, (aIdx + 2), Zero ps')

    static member rev : AltBinRndAccList<'a> -> AltBinRndAccList<'a> = function
        | Nil -> Nil
        | xs -> 
            let rec loop xs' acc =
                match (AltBinRndAccList.tryUncons xs') with
                | None -> acc
                | Some(x, xs'') -> loop xs'' (AltBinRndAccList.cons x acc)
            loop xs Nil

    static member append : AltBinRndAccList<'a> * AltBinRndAccList<'a> -> AltBinRndAccList<'a> = function
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

    static member length : int * int * AltBinRndAccList<'a> -> int = function
        | len, acc, Nil -> len
        | len, acc, One(x, Nil) -> len + acc
        | len, acc, One(x, ps) -> AltBinRndAccList.length ((len + acc), (2 * acc), ps)
        | len, acc, Zero ps -> AltBinRndAccList.length (len, (2 * acc), ps)
               
let empty = Nil

let isEmpty = function Nil -> true | _ -> false

let inline cons x xs = AltBinRndAccList.cons x xs

let inline uncons x = AltBinRndAccList.uncons x
let inline tryUncons x = AltBinRndAccList.tryUncons x

let head xs =
    let x, _ = uncons xs
    x

let tryGetHead xs =
    match (tryUncons xs) with
    | None -> None
    | Some(x, _) -> Some(x)

let tail xs =
    let _, xs' = uncons xs
    xs'

let tryGetTail xs =
    match (tryUncons xs) with
    | None -> None
    | Some( _, xs') -> Some(xs')

let rec lookup i xs = AltBinRndAccList.lookup i xs
let rec tryLookup i xs = AltBinRndAccList.tryLookup i xs

let inline length xs = AltBinRndAccList.length (0, 1, xs)
let inline update i y xs = AltBinRndAccList.fupdate ((fun x -> y), i, xs)
let inline tryUpdate i y xs = AltBinRndAccList.tryfUpdate ((fun x -> y), i, xs)
let inline rev xs = AltBinRndAccList.rev xs
let inline append xs ys = AltBinRndAccList.append (xs, ys)

let remove i xs = 
    if i = 0 then 
        match (AltBinRndAccList.uncons xs) with
        | _, Zero(Nil) -> Nil
        | _, x -> x
    else
        let front = Array.create i (fst (AltBinRndAccList.uncons xs))
        match (AltBinRndAccList.remove (i, front, 0, xs)) with
        | _, Zero(Nil) -> Nil
        | frontLen, x -> 
            let rec loop i' (front':'a array) (back:AltBinRndAccList<'a>) =
                match i' with
                    
                | i'' when i'' > -1 -> loop (i''- 1) front' ( AltBinRndAccList.cons front'.[i''] back)
                | i'' -> back
                        
            loop (frontLen - 1) front x

let tryRemove i xs = 
    if i = 0 then 
        match (AltBinRndAccList.uncons xs) with
        | _, Zero(Nil) -> Some(Nil)
        | _, x -> Some(x)
    else
        let front = Array.create i (fst (AltBinRndAccList.uncons xs))
        match (AltBinRndAccList.tryRemove (i, front, 0, xs)) with
        | _, None -> None
        | _, Some(Zero(Nil)) -> Some(Nil)
        | frontLen, Some(x) -> 
            let rec loop i' (front':'a array) (back:AltBinRndAccList<'a>) =
                match i' with
                    
                | i'' when i'' > -1 -> loop (i''- 1) front' ( AltBinRndAccList.cons front'.[i''] back)
                | i'' -> Some(back)
                        
            loop (frontLen - 1) front x