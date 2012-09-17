// TreeZipper
// original implementation taken from http://blog.xquant.net/?p=156
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpx.DataStructures.TreeZipper

/// A simple binarys tree
type 'a Tree=
   | Leaf
   | Branch of 'a * 'a Tree * 'a Tree

type TreeDirection =  Left | Right
type ZipperDirection = Up | Left | Right 

/// The tree zipper datastructure
type 'a TreeZipper  = { 
  Focus:'a Tree
  Path: (TreeDirection * 'a * 'a Tree) list }

open FSharpx

/// Moves the zipper one level up
let up z =
    match z.Path with
    | (d,v,other)::ep ->  
        match d with 
        | TreeDirection.Left  -> { Focus = Branch(v,z.Focus,other); Path = ep }
        | TreeDirection.Right -> { Focus = Branch(v,other,z.Focus); Path = ep }
    | [] -> failwith "can't go up" // because ep only goes down and is empty 

/// Moves the zipper to the top
let rec top z  = 
    match z.Path with 
    | [] -> z 
    | _  -> top (up z)

/// Moves the zipper to the left
let left z = 
    match z.Focus with
    | (Branch(v, explored, other)) -> { Focus = explored; Path = (TreeDirection.Left, v, other)::z.Path }
    | Leaf -> failwith "can't go down on leaf"

/// Moves the zipper to the right
let right z = 
    match z.Focus with
    | (Branch(v, other, explored)) -> { Focus = explored; Path= (TreeDirection.Right, v, other)::z.Path }
    | Leaf -> failwith "can't go down on leaf"

let inline getMove direction =
    match direction with
    | Up -> up
    | Left -> left
    | Right -> right
    
/// Moves the zipper in the directions of the given list
let rec move directions (z:'a TreeZipper) =
    directions
      |> Seq.map getMove
      |> Seq.fold (|>) z
        
/// Creates a zipper from a tree
let zipper t = { Focus = t; Path = [] } 