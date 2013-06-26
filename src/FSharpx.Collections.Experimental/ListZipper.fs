/// ListZipper
/// original implementation taken from http://learnyouahaskell.com/zippers
module FSharpx.Collections.Experimental.ListZipper

#nowarn "25"
/// A zipper for lists
type ListZipper<'T> = { Focus : 'T list; Path : 'T list } 

/// Returns the head element from the list under focus
let focus zipper =
    match zipper.Focus with
    | x::_ -> x

/// Changes the element under the focus
let modify newElement zipper =
    match zipper.Focus with
    | x::xs -> { zipper with Focus = newElement::xs } 

/// Moves the zipper forward
let forward zipper = 
    match zipper.Focus with
    | x::xs -> { Focus = xs; Path = x::zipper.Path }

/// Moves the zipper backwards
let back zipper = 
    match zipper.Path with
    | b::bs -> { Focus = b::zipper.Focus; Path = bs }

/// Moves the zipper to the front
let rec front zipper =
    match zipper.Path with
    | [] -> zipper
    | _ -> back zipper |> front

/// Creates a list zipper
let zipper list = { Focus = list; Path = [] }

/// Returns the whole list from the zipper
let getList zipper = (front zipper).Focus