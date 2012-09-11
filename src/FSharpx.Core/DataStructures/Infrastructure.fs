namespace FSharpx.DataStructures

module Exceptions = 
    let Empty = new System.Exception("Queue is empty") // TODO: make this a better exception

    let OutOfBounds = new System.IndexOutOfRangeException() // TODO: make this a better exception

module LazyList =    
    let rec private revAux r acc =
        match r with
        | LazyList.Nil -> acc
        | LazyList.Cons(hd, tl) -> revAux tl (LazyList.cons hd acc)

    let lLrev r =
        revAux r LazyList.empty

    let rec lLdrop n xs =
        if n < 0 then invalidArg "n" "n was negative"
        elif n > 0 then
            match xs with
            | LazyList.Cons(x, xs') -> lLdrop (n-1) xs'
            | _ -> LazyList.empty
        else
            xs

    let lLsplit (ll:LazyList<'a>) n  =
        let rec loop z (leftL:'a List) (ll':LazyList<'a>) = 
            match z with
            | 0 -> leftL, (LazyList.tail ll')
            | _ -> loop (z - 1)  ((LazyList.head ll')::leftL) (LazyList.tail ll')
        loop n List.empty ll