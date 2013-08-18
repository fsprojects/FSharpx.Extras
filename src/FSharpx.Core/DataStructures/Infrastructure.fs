namespace FSharpx.DataStructures

open FSharpx.Collections

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
module Exceptions = 
    let Empty = new System.Exception("Queue is empty") // TODO: make this a better exception

    let OutOfBounds = new System.IndexOutOfRangeException() // TODO: make this a better exception

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
module LazyList =
    
    let rec private revAux r acc =
        match r with
        | LazyList.Nil -> acc
        | LazyList.Cons(hd, tl) -> revAux tl (LazyList.cons hd acc)

    let rev r =
        revAux r LazyList.empty

    let rec drop n xs =
        if n < 0 then invalidArg "n" "n was negative"
        elif n > 0 then
            match xs with
            | LazyList.Cons(x, xs') -> drop (n-1) xs'
            | _ -> LazyList.empty
        else
            xs

    let split (ll:LazyList<'a>) n  =
        let rec loop z (leftL:'a List) (ll':LazyList<'a>) = 
            match z with
            | 0 -> leftL, (LazyList.tail ll')
            | _ -> loop (z - 1)  ((LazyList.head ll')::leftL) (LazyList.tail ll')
        loop n List.empty ll

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
module ListHelpr =

    let rec loop2Array (left:'a array) right = function
        | x when x < 0 -> left, (List.tail right)
        | x ->  
            Array.set left x (List.head right)
            loop2Array left (List.tail right) (x-1) 

    let rec loopFromArray frontLen (left:'a array) right = function
        | x when x > frontLen -> right
        | x -> loopFromArray frontLen left (left.[x]::right) (x + 1) 
