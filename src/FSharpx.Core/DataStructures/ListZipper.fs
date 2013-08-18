/// ListZipper
/// original implementation taken from http://learnyouahaskell.com/zippers
[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
module FSharpx.DataStructures.ListZipper

#nowarn "44"
/// A zipper for lists
[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
type 'a ListZipper = { Focus : 'a list; Path : 'a list } 

/// Returns the head element from the list under focus
let focus zipper =
    match zipper.Focus with
    | x::_ -> x
    | _ -> invalidArg "zipper" "invalid"

/// Changes the element under the focus
let modify newElement zipper =
    match zipper.Focus with
    | x::xs -> { zipper with Focus = newElement::xs } 
    | _ -> invalidArg "zipper" "invalid"

/// Moves the zipper forward
let forward zipper = 
    match zipper.Focus with
    | x::xs -> { Focus = xs; Path = x::zipper.Path }
    | _ -> invalidArg "zipper" "invalid"

/// Moves the zipper backwards
let back zipper = 
    match zipper.Path with
    | b::bs -> { Focus = b::zipper.Focus; Path = bs }
    | _ -> invalidArg "zipper" "invalid"

/// Moves the zipper to the front
let rec front zipper =
    match zipper.Path with
    | [] -> zipper
    | _ -> back zipper |> front

/// Creates a list zipper
let zipper list = { Focus = list; Path = [] }

/// Returns the whole list from the zipper
let getList zipper = (front zipper).Focus