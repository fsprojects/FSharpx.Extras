// Copyright 2010-2013, as indicated in README.md in the root directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 (the "License")

namespace FSharpx.Collections.Experimental

open FSharpx.Collections

module Exceptions = 
    let Empty = new System.Exception("Queue is empty") // TODO: make this a better exception

    let OutOfBounds = new System.IndexOutOfRangeException() // TODO: make this a better exception

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

    let split (ll:LazyList<'T>) n  =
        let rec loop z (leftL:'T List) (ll':LazyList<'T>) = 
            match z with
            | 0 -> leftL, (LazyList.tail ll')
            | _ -> loop (z - 1)  ((LazyList.head ll')::leftL) (LazyList.tail ll')
        loop n List.empty ll

module internal ListHelpr =

    let rec loop2Array (left:'T array) right = function
        | x when x < 0 -> left, (List.tail right)
        | x ->  
            Array.set left x (List.head right)
            loop2Array left (List.tail right) (x-1) 

    let rec loopFromArray frontLen (left:'T array) right = function
        | x when x > frontLen -> right
        | x -> loopFromArray frontLen left (left.[x]::right) (x + 1) 
