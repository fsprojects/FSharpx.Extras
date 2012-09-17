// TreeZipper
// original implementation taken from http://blog.xquant.net/?p=156
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpx.DataStructures.TreeZipper

type 'a Tree=
   | Leaf
   | Branch of  'a * 'a Tree * 'a Tree

type TDirection =  Left | Right
type ZDirection = Up | Left | Right 

type 'a Zipper  = { focus:'a Tree;  path: (TDirection * 'a * 'a Tree) list}

open FSharpx

let up z =  match z.path with
            | (d,v,other)::ep ->  match d with 
                                    | TDirection.Left  -> {focus=Branch(v,z.focus,other); path=ep}
                                    | TDirection.Right -> {focus=Branch(v,other,z.focus); path=ep}
            | []        -> failwith "can't go up" // because ep only goes down and is empty 

let rec top z  =  match z.path with [] -> z | _  -> top (up z)

let left z  =   match z.focus with
                        | (Branch(v, l, r)) ->  let v, explored, other = v, l, r 
                                                {focus=explored; path=((TDirection.Left, v, other)::z.path)}
                        | Leaf -> failwith "can't go down on leaf"

let right z =  match z.focus with
                        | (Branch(v, l, r)) ->  let v, explored, other = v, r, l 
                                                {focus=explored; path=((TDirection.Right, v, other)::z.path)}
                        | Leaf -> failwith "can't go down on leaf"

let rec move (p:ZDirection list) (z:'a Zipper) =
    match p with 
    | []    -> z 
    | d::xs -> match d with 
                | Up    -> move xs (up z)
                | Left  -> move xs (left z)
                | Right -> move xs (right z)
let fromTree t = {focus = t; path = []} 