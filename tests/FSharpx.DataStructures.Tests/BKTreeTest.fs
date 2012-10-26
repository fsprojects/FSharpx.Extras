module FSharpx.DataStructures.Tests.BKTreeTest

open System.Collections.Generic
open FSharpx
open FSharpx.DataStructures
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

let fsCheck t = fsCheck "" t

let sem tree = tree |> BKTree.elems |> List.sort

let trans f xs = xs |> BKTree.Int.fromList |> f |> sem

let rec inv dict = function
    | Empty -> true
    | Node(a, _, dictionary) ->
        List.forall (fun (d,b) -> BKTree.Int.distance a b = d) dict &&
            List.forall (fun (d,t) -> inv ((d,a)::dict) t) (Dictionary.toList dictionary)

let invariant t = inv [] t

[<Test>]
let ``native empty``() =
    fsCheck <| fun (xs :int list) ->
        BKTree.List.distance [] xs = List.length xs && BKTree.List.distance xs [] = List.length xs

[<Test>]
let ``native cons``() =
    fsCheck <| fun (x:int) xs ys ->
        BKTree.List.distance (x::xs) (x::ys) = BKTree.List.distance xs ys

[<Test>]
let ``native diff``() =
    fsCheck <| fun (x:int) y xs ys ->
        x <> y ==>
            (BKTree.List.distance (x::xs) (y::ys) =
              1 + List.min [BKTree.List.distance (x::xs) ys; BKTree.List.distance (x::xs) (x::ys); BKTree.List.distance xs (y::ys)])

[<Test>]
let empty() =
    fsCheck (fun n -> not <| BKTree.Int.exists n BKTree.empty)

[<Test>]
let isEmpty() =
    fsCheck (fun xs -> BKTree.isEmpty (BKTree.Int.fromList xs) = List.isEmpty xs)

[<Test>]
let singleton() =
    fsCheck (fun n -> BKTree.elems (BKTree.Int.fromList [n]) = [n])

[<Test>]
let fromList() =
    fsCheck (fun xs -> sem (BKTree.Int.fromList xs) = List.sort xs)

[<Test>]
let ``fromList inv``() =
    fsCheck (fun xs -> invariant (BKTree.Int.fromList xs))

[<Test>]
let insert() =
    fsCheck (fun n xs -> trans (BKTree.Int.insert n) xs = List.sort (n::xs))

[<Test>]
let ``insert inv``() =
    fsCheck (fun n xs -> invariant (BKTree.Int.insert n (BKTree.Int.fromList xs)))

[<Test>]
let exists() =
    fsCheck (fun n xs -> BKTree.Int.exists n (BKTree.Int.fromList xs) = List.exists ((=) n) xs)

[<Test>]
let existsDistance() =
    fsCheck <| fun dist n xs ->
        let d = dist % 5
        let reference = List.exists (fun e -> BKTree.Int.distance n e <= d) xs
        Prop.collect reference
            (BKTree.Int.existsDistance d n (BKTree.Int.fromList xs) = List.exists (fun e -> BKTree.Int.distance n e <= d) xs)

[<Test>]
let delete() =
    let rec removeFirst n = function
        | [] -> []
        | x::xs when x = n -> xs
        | x::xs -> x :: removeFirst n xs
    fsCheck (fun n xs -> trans (BKTree.Int.delete n) xs = List.sort (removeFirst n xs))
     
[<Test>]
let ``delete inv``() =
    fsCheck (fun n xs -> invariant (BKTree.Int.delete n (BKTree.Int.fromList xs)))

[<Test>]
let elems() =
    fsCheck (fun xs -> List.sort (BKTree.elems (BKTree.Int.fromList xs)) = List.sort xs)

[<Test>]
let elemsDistance() =
    fsCheck <| fun dist n xs ->
        let d = dist % 5
        List.sort (BKTree.Int.elemsDistance d n (BKTree.Int.fromList xs)) =
            List.sort (List.filter (fun e -> BKTree.Int.distance n e <= d) xs)

[<Test>]
let unions() =
    fsCheck (fun xss -> sem (BKTree.Int.unions (List.map BKTree.Int.fromList xss)) = List.sort (List.concat xss))

[<Test>]
let ``unions inv``() =
    fsCheck (fun xss -> invariant (BKTree.Int.unions (List.map BKTree.Int.fromList xss)))

[<Test>]
let union() =
    fsCheck <| fun xs ys ->
        sem (BKTree.Int.union (BKTree.Int.fromList xs) (BKTree.Int.fromList ys)) = List.sort (List.append xs ys)

[<Test>]
let ``union inv``() =
    fsCheck (fun xs ys -> invariant (BKTree.Int.union (BKTree.Int.fromList xs) (BKTree.Int.fromList ys)))

[<Test>]
let ``delete . insert = id``() =
    fsCheck (fun n xs -> trans ((BKTree.Int.delete n) << (BKTree.Int.insert n)) xs = List.sort xs)

[<Test>]
let ``The size of an empty BKTree is 0``() =
    fsCheck (fun _ -> BKTree.size BKTree.empty = 0)

[<Test>]
let ``fromList and size``() =
    fsCheck (fun xs -> BKTree.size (BKTree.Int.fromList xs) = List.length xs)

[<Test>]
let ``insert . size = size + 1``() =
    fsCheck <| fun n xs ->
        let tree = BKTree.Int.fromList xs
        BKTree.size (BKTree.Int.insert n tree) = BKTree.size tree + 1

[<Test>]
let ``delete . size = size - 1``() =
    fsCheck <| fun n xs ->
        let tree = BKTree.Int.fromList xs
        BKTree.size (BKTree.Int.delete n tree) = BKTree.size tree - if BKTree.Int.exists n tree then 1 else 0

[<Test>]
let ``union and size``() =
    fsCheck <| fun xs ys ->
        let treeXs = BKTree.Int.fromList xs
        let treeYs = BKTree.Int.fromList ys
        BKTree.size (BKTree.Int.union treeXs treeYs) = BKTree.size treeXs + BKTree.size treeYs

[<Test>]
let ``unions and size``() =
    fsCheck <| fun xss ->
        let trees = List.map BKTree.Int.fromList xss
        BKTree.size (BKTree.Int.unions trees) = List.sum (List.map BKTree.size trees)

[<Test>]
let ``unions and exists``() =
    fsCheck <| fun xss ->
        let tree = BKTree.Int.unions (List.map BKTree.Int.fromList xss)
        List.forall (fun x -> BKTree.Int.exists x tree) (List.concat xss)

[<Test>]
let ``fromList and exists``() =
    fsCheck <| fun xs ->
        let tree = BKTree.Int.fromList xs
        List.forall (fun x -> BKTree.Int.exists x tree) xs
