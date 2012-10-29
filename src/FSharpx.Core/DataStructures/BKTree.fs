namespace FSharpx.DataStructures

// Ported from http://hackage.haskell.org/packages/archive/bktrees/latest/doc/html/src/Data-Set-BKTree.html

type 'a BKTree =
  | Node of 'a * int * Map<int,'a BKTree>
  | Empty

module BKTree =

    open FSharpx

    let isEmpty = function
        | Empty -> true
        | Node(_,_,_) -> false

    let empty = Empty

    let size = function
        | Node(_, s, _) -> s
        | Empty -> 0

    let singleton a = Node(a, 1, Map.empty)

    let rec insert distance a = function
        | Empty -> Node(a, 1 , Map.empty)
        | Node(b, size, dict) ->
            let recurse _ tree = insert distance a tree
            let d = distance a b
            let dict = Map.insertWith recurse d (Node(a, 1, Map.empty)) dict
            Node(b, size + 1, dict)

    let rec exists distance a = function
        | Empty -> false
        | Node(b, _, dict) ->
            let d = distance a b
            if d = 0 then true
            else
                match Map.tryFind d dict with
                | None -> false
                | Some tree -> exists distance a tree

    let private subTree d n dict =
        let (_, rightTree) = dict |> Map.splitWithKey ((>) (d-n-1))
        let (centerTree, _) = rightTree |> Map.splitWithKey ((>=) (d+n+1))
        centerTree

    let rec existsDistance distance n a = function
        | Empty -> false
        | Node(b, _, map) ->
            let d = distance a b
            if d <= n then true
            else
                map
                |> subTree d n
                |> Map.valueList
                |> List.exists (existsDistance distance n a)

    let rec elems = function
        | Empty -> []
        | Node(a, _ , d) ->
            a :: (d |> Map.valueList |> List.collect elems)

    let rec elemsDistance distance n a = function
        | Empty -> []
        | Node(b, _, dict) ->
            let d = distance a b
            dict
            |> subTree d n
            |> Map.valueList
            |> List.collect (elemsDistance distance n a)
            |> if d <= n then List.cons b else id

    let fromList distance xs = xs |> List.fold (flip (insert distance)) empty

    let unions distance xs = xs |> List.collect elems |> fromList distance

    let rec delete distance a = function
        | Empty -> Empty
        | Node(b, _, map) ->
            let d = distance a b
            if d = 0 then unions distance (Map.valueList map)
            else
                let subtree = Map.updateWith (Some << (delete distance a)) d map
                let size = subtree |> Map.valueList |> List.map size |> List.sum |> (+) 1
                Node(b, size, subtree)

    module Int =

        let distance i j = abs(i - j)

        let insert a tree = insert distance a tree

        let exists a tree = exists distance a tree
  
        let existsDistance n a tree = existsDistance distance n a tree

        let elemsDistance n a tree = elemsDistance distance n a tree

        let fromList xs = fromList distance xs

        let unions xs = unions distance xs

        let union t1 t2 = unions [t1; t2]

        let delete a tree = delete distance a tree

    module Char =

        let distance(i:char) (j:char) = abs((int i) - (int j))

        let insert a tree = insert distance a tree

        let exists a tree = exists distance a tree

        let existsDistance n a tree = existsDistance distance n a tree

        let elemsDistance n a tree = elemsDistance distance n a tree

        let fromList xs = fromList distance xs

        let unions xs = unions distance xs

        let union t1 t2 = unions [t1; t2]

        let delete a tree = delete distance a tree

    module List =

        let private hirschberg xs = function
            | [] -> List.length xs
            | ys ->
                let lys = List.length ys
                let startArr = [1..lys] |> List.map (fun x -> (x,x))
                xs
                |> Seq.zip (Seq.unfold (fun i -> Some(i+1, i+1)) 0)
                |> Seq.toList
                |> List.fold (fun arr (i,xi) ->
                    ys
                    |> List.zip (List.sortBy fst arr)
                    |> List.mapAccum (fun (s,c) ((j,el),yj) ->
                        let nc = List.min [s + (if xi = yj then 0 else 1); el + 1; c + 1]
                        ((el,nc),(j,nc))
                    ) (i - 1, i)
                    |> snd
                ) startArr
                |> List.find (fst >> (=) lys)
                |> snd

        let distance = hirschberg

        let insert a tree = insert distance a tree

        let exists a tree = exists distance a tree

        let existsDistance n a tree = existsDistance distance n a tree

        let elemsDistance n a tree = elemsDistance distance n a tree

        let fromList xs = fromList distance xs

        let unions xs = unions distance xs

        let union t1 t2 = unions [t1; t2]

        let delete a tree = delete distance a tree

    module ByteString =

        open ByteString

        let distance xs = function
            | ys when ByteString.isEmpty ys -> ByteString.length xs
            | ys ->
                let xs = ByteString.toList xs
                let ys = ByteString.toList ys
                List.distance xs ys

        let insert a tree = insert distance a tree

        let exists a tree = exists distance a tree

        let existsDistance n a tree = existsDistance distance n a tree

        let elemsDistance n a tree = elemsDistance distance n a tree

        let fromList xs = fromList distance xs

        let unions xs = unions distance xs

        let union t1 t2 = unions [t1; t2]

        let delete a tree = delete distance a tree
