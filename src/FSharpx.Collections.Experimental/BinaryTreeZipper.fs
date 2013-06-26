/// TreeZipper
/// original implementation taken from http://blog.xquant.net/?p=156
module FSharpx.Collections.Experimental.BinaryTreeZipper

/// A simple binary tree
type BinaryTree<'T> =
   | Leaf
   | Branch of 'T * BinaryTree<'T> * BinaryTree<'T>


/// Creates a new branch with the label x and two leafs as subbranches
let branch x = Branch(x,Leaf,Leaf)

type TreeDirection =  Left | Right

/// The zipper datastructure for binary trees
type BinaryTreeZipper<'T> = { 
  Focus:BinaryTree<'T>
  Path: (TreeDirection * 'T * BinaryTree<'T>) list }

open FSharpx

/// Moves the zipper one level up
let up z =
    match z.Path with
    | (Left,v,other)::ep ->  { Focus = Branch(v,z.Focus,other); Path = ep }
    | (Right,v,other)::ep -> { Focus = Branch(v,other,z.Focus); Path = ep }
    | [] -> failwith "can't go up" // because ep only goes down and is empty 

/// Moves the zipper to the top
let rec top z  = 
    match z.Path with 
    | [] -> z 
    | _  -> top (up z)

/// Moves the zipper to the left
let left z = 
    match z.Focus with
    | (Branch(v, explored, other)) -> { Focus = explored; Path = (Left, v, other) :: z.Path }
    | Leaf -> failwith "can't go down on leaf"

/// Moves the zipper to the right
let right z = 
    match z.Focus with
    | (Branch(v, other, explored)) -> { Focus = explored; Path = (Right, v, other) :: z.Path }
    | Leaf -> failwith "can't go down on leaf"

/// Modifies the current focus inside the zipper
let setFocus newFocus zipper = { zipper with Focus = newFocus }

/// Creates a zipper from a tree
let zipper t = { Focus = t; Path = [] } 

type TreeZipperDirection = Up | Left | Right 

let inline getMove direction =
    match direction with
    | Up -> up
    | Left -> left
    | Right -> right
    
/// Moves the zipper in the directions of the given list
let rec move directions (z:BinaryTreeZipper<'T>) =
    directions
      |> Seq.map getMove
      |> Seq.fold (|>) z       