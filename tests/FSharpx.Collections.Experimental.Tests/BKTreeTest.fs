module FSharpx.Collections.Experimental.Tests.BKTreeTest

open FSharpx
open FSharpx.Collections.Experimental
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

let fsCheck t = fsCheck "" t

let sem tree = tree |> BKTree.toList |> List.sort

let trans f xs = xs |> BKTree.Int.ofList |> f |> sem

let rec inv m = function
    | Empty -> true
    | Node(a, _, map) ->
        List.forall (fun (d,b) -> BKTree.Int.distance a b = d) m &&
            List.forall (fun (d,t) -> inv ((d,a)::m) t) (IntMap.toList map)

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
    fsCheck (fun xs -> BKTree.isEmpty (BKTree.Int.ofList xs) = List.isEmpty xs)

[<Test>]
let singleton() =
    fsCheck (fun n -> BKTree.toList (BKTree.Int.ofList [n]) = [n])

[<Test>]
let ofList() =
    fsCheck (fun xs -> sem (BKTree.Int.ofList xs) = List.sort xs)

[<Test>]
let ``ofList inv``() =
    fsCheck (fun xs -> invariant (BKTree.Int.ofList xs))

[<Test>]
let add() =
    fsCheck (fun n xs -> trans (BKTree.Int.add n) xs = List.sort (n::xs))

[<Test>]
let ``add inv``() =
    fsCheck (fun n xs -> invariant (BKTree.Int.add n (BKTree.Int.ofList xs)))

[<Test>]
let exists() =
    fsCheck (fun n xs -> BKTree.Int.exists n (BKTree.Int.ofList xs) = List.exists ((=) n) xs)

[<Test>]
let existsDistance() =
    fsCheck <| fun dist n xs ->
        let d = dist % 5
        let reference = List.exists (fun e -> BKTree.Int.distance n e <= d) xs
        Prop.collect reference
            (BKTree.Int.existsDistance d n (BKTree.Int.ofList xs) = List.exists (fun e -> BKTree.Int.distance n e <= d) xs)

[<Test>]
let delete() =
    let rec removeFirst n = function
        | [] -> []
        | x::xs when x = n -> xs
        | x::xs -> x :: removeFirst n xs
    fsCheck (fun n xs -> trans (BKTree.Int.delete n) xs = List.sort (removeFirst n xs))
     
[<Test>]
let ``delete inv``() =
    fsCheck (fun n xs -> invariant (BKTree.Int.delete n (BKTree.Int.ofList xs)))

[<Test>]
let toList() =
    fsCheck (fun xs -> List.sort (BKTree.toList (BKTree.Int.ofList xs)) = List.sort xs)

[<Test>]
let toListDistance() =
    fsCheck <| fun dist n xs ->
        let d = dist % 5
        List.sort (BKTree.Int.toListDistance d n (BKTree.Int.ofList xs)) =
            List.sort (List.filter (fun e -> BKTree.Int.distance n e <= d) xs)

[<Test>]
let concat() =
    fsCheck (fun xss -> sem (BKTree.Int.concat (List.map BKTree.Int.ofList xss)) = List.sort (List.concat xss))

[<Test>]
let ``concat inv``() =
    fsCheck (fun xss -> invariant (BKTree.Int.concat (List.map BKTree.Int.ofList xss)))

[<Test>]
let append() =
    fsCheck <| fun xs ys ->
        sem (BKTree.Int.append (BKTree.Int.ofList xs) (BKTree.Int.ofList ys)) = List.sort (List.append xs ys)

[<Test>]
let ``append inv``() =
    fsCheck (fun xs ys -> invariant (BKTree.Int.append (BKTree.Int.ofList xs) (BKTree.Int.ofList ys)))

[<Test>]
let ``delete . add = id``() =
    fsCheck (fun n xs -> trans ((BKTree.Int.delete n) << (BKTree.Int.add n)) xs = List.sort xs)

[<Test>]
let ``The size of an empty BKTree is 0``() =
    fsCheck (fun _ -> BKTree.size BKTree.empty = 0)

[<Test>]
let ``ofList and size``() =
    fsCheck (fun xs -> BKTree.size (BKTree.Int.ofList xs) = List.length xs)

[<Test>]
let ``add . size = size + 1``() =
    fsCheck <| fun n xs ->
        let tree = BKTree.Int.ofList xs
        BKTree.size (BKTree.Int.add n tree) = BKTree.size tree + 1

[<Test>]
let ``delete . size = size - 1``() =
    fsCheck <| fun n xs ->
        let tree = BKTree.Int.ofList xs
        BKTree.size (BKTree.Int.delete n tree) = BKTree.size tree - if BKTree.Int.exists n tree then 1 else 0

[<Test>]
let ``append and size``() =
    fsCheck <| fun xs ys ->
        let treeXs = BKTree.Int.ofList xs
        let treeYs = BKTree.Int.ofList ys
        BKTree.size (BKTree.Int.append treeXs treeYs) = BKTree.size treeXs + BKTree.size treeYs

[<Test>]
let ``concat and size``() =
    fsCheck <| fun xss ->
        let trees = List.map BKTree.Int.ofList xss
        BKTree.size (BKTree.Int.concat trees) = List.sum (List.map BKTree.size trees)

[<Test>]
let ``concat and exists``() =
    fsCheck <| fun xss ->
        let tree = BKTree.Int.concat (List.map BKTree.Int.ofList xss)
        List.forall (fun x -> BKTree.Int.exists x tree) (List.concat xss)

[<Test>]
let ``ofList and exists``() =
    fsCheck <| fun xs ->
        let tree = BKTree.Int.ofList xs
        List.forall (fun x -> BKTree.Int.exists x tree) xs

[<Test>]
let ``implements IEnumerable``() =
    fsCheck <| fun values ->
        let tree = BKTree.Int.ofList values
        let a = tree :> _ seq |> Seq.toList
        set values = set a && a.Length = values.Length
