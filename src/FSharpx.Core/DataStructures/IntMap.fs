namespace FSharpx.DataStructures

// Ported from http://hackage.haskell.org/packages/archive/containers/latest/doc/html/src/Data-IntMap-Base.html

#nowarn "44"
open System.Collections
open System.Collections.Generic
open FSharpx
open FSharpx.Collections

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
type 'a IntMap =
    | Nil
    | Tip of int * 'a
    | Bin of int * int * 'a IntMap * 'a IntMap
    with
    member x.FoldBackWithKey f z =
        let rec go z =
            function
            | Nil -> z
            | Tip(kx, x) -> f kx x z
            | Bin(_, _, l, r) -> go (go z r) l
        match x with
        | Bin(_, m, l, r) when m < 0 -> go (go z l) r  // put negative numbers before.
        | Bin(_, m, l, r) -> go (go z r) l
        | _ -> go z x
    
    member x.ToList() = x.FoldBackWithKey (fun k x xs -> (k, x) :: xs) []

    interface IEnumerable<int * 'a> with
        member x.GetEnumerator() =
            (x.ToList() :> (_ * _) seq).GetEnumerator()
        
        member x.GetEnumerator() =
            (x :> _ seq).GetEnumerator() :> IEnumerator

[<System.Obsolete("Namespace 'FSharpx.DataStructures' obsolete. Use 'FSharpx.Collections' or 'FSharpx.Collections.Experimental'  instead.")>]
module IntMap =

    let inline private maskW i m = int (i &&& (~~~ (m - 1ul) ^^^ m))
    let inline private mask i m = maskW (uint32 i) (uint32 m)
    let inline private match' i p m = mask i m = p
    let inline private nomatch i p m = mask i m <> p
    let inline private zero i m = (uint32 i) &&& (uint32 m) = 0ul
    let inline private shorter m1 m2 = (uint32 m1) > (uint32 m2)

    let inline private highestBitMask x0 =
        let x1 = x0 ||| (x0 >>> 1)
        let x2 = x1 ||| (x1 >>> 2)
        let x3 = x2 ||| (x2 >>> 4)
        let x4 = x3 ||| (x3 >>> 8)
        let x5 = x4 ||| (x4 >>> 16)
        let x6 = x5 ||| (x5 >>> 32)   // for 64 bit platforms
        x6 ^^^ (x6 >>> 1)

    let inline private branchMask p1 p2 = int (highestBitMask (uint32 p1 ^^^ uint32 p2))

    let inline private join p1 t1 p2 t2 =
        let m = branchMask p1 p2
        let p = mask p1 m
        if zero p1 m then Bin(p, m, t1, t2)
        else Bin(p, m, t2, t1)

    let inline private bin p m l r =
        match l, r with
        | (l, Nil) -> l
        | (Nil, r) -> r
        | (l, r) -> Bin(p, m, l, r)

    ///O(1). Map is empty.  Credit: Haskell.org
    let isEmpty =
        function
        | Nil -> true
        | _ -> false

    ///O(n). Number of elements in the map. Credit: Haskell.org
    let rec size =
        function
        | Bin(_, _, l, r) -> size l + size r
        | Tip _ -> 1
        | Nil -> 0

    ///O(min(n,W)). Lookup the value at a key in the map. Returns 'a option. Credit: Haskell.org
    let rec tryFind k =
        function
        | Bin(p, m, l, r) when nomatch k p m -> None
        | Bin(p, m, l, r) when zero k m -> tryFind k l
        | Bin(p, m, l, r) -> tryFind k r
        | Tip(kx, x) when k = kx -> Some x
        | Tip(kx, x) -> None
        | Nil -> None

    ///O(min(n,W)). Is the key a member of the map? Credit: Haskell.org
    let rec exists k =
        function
        | Bin(p, m, l, r) when nomatch k p m -> false
        | Bin(p, m, l, r) when zero k m -> exists k l
        | Bin(p, m, l, r) -> exists k r
        | Tip(kx, _) -> k = kx
        | Nil -> false

    ///O(log n). Is the key not a member of the map? Credit: Haskell.org
    let notExists k m = not <| exists k m

    ///O(min(n,W)). Lookup the value at a key in the map. Credit: Haskell.org
    let rec find k m =
        let notFound() = failwith <| sprintf "IntMap.find: key %d is not an element of the map" k
        match m with
        | Bin(p, m, l, r) when nomatch k p m -> notFound()
        | Bin(p, m, l, r) when zero k m -> find k l
        | Bin(p, m, l, r) -> find k r
        | Tip(kx, x) when k = kx -> x
        | Tip(kx, x) -> notFound()
        | Nil -> notFound()

    ///O(min(n,W)). The expression (findWithDefault def k map) returns the value at key k or returns def when the key is not an element of the map.  Credit: Haskell.org
    let rec findWithDefault def k =
        function
        | Bin(p, m, l, r) when nomatch k p m -> def
        | Bin(p, m, l, r) when zero k m -> findWithDefault def k l
        | Bin(p, m, l, r) -> findWithDefault def k r
        | Tip(kx, x) when k = kx -> x
        | Tip(kx, x) -> def
        | Nil -> def

    let rec private unsafeFindMax =
        function
        | Nil -> None
        | Tip(ky, y) -> Some(ky, y)
        | Bin(_, _, _, r) -> unsafeFindMax r

    ///O(log n). Find largest key smaller than the given one and return the corresponding (key, value) pair.  Credit: Haskell.org
    let tryFindLT k t =
        let rec go def =
            function
            | Bin(p, m, l, r) when nomatch k p m -> if k < p then unsafeFindMax def else unsafeFindMax r
            | Bin(p, m, l, r) when zero k m -> go def l
            | Bin(p, m, l, r) -> go l r
            | Tip(ky, y) when k <= ky -> unsafeFindMax def
            | Tip(ky, y) -> Some(ky, y)
            | Nil -> unsafeFindMax def
        match t with
        | Bin(_, m, l, r) when m < 0 -> if k >= 0 then go r l else go Nil r
        | _ -> go Nil t

    let rec private unsafeFindMin =
        function
        | Nil -> None
        | Tip(ky, y) -> Some(ky, y)
        | Bin(_, _, l, _) -> unsafeFindMin l

    ///O(log n). Find smallest key greater than the given one and return the corresponding (key, value) pair. Credit: Haskell.org
    let tryFindGT k t =
        let rec go def =
            function
            | Bin(p, m, l, r) when nomatch k p m -> if k < p then unsafeFindMin l else unsafeFindMin def
            | Bin(p, m, l, r) when zero k m -> go r l
            | Bin(p, m, l, r) -> go def r
            | Tip(ky, y) when k >= ky -> unsafeFindMin def
            | Tip(ky, y) -> Some(ky, y)
            | Nil -> unsafeFindMin def
        match t with
        | Bin(_, m, l, r) when m < 0 -> if k >= 0 then go Nil l else go l r
        | _ -> go Nil t

    ///O(log n). Find largest key smaller or equal to the given one and return the corresponding (key, value) pair. Credit: Haskell.org
    let tryFindLE k t =
        let rec go def =
            function
            | Bin(p, m, l, r) when nomatch k p m -> if k < p then unsafeFindMax def else unsafeFindMax r
            | Bin(p, m, l, r) when zero k m -> go def l
            | Bin(p, m, l, r) -> go l r
            | Tip(ky, y) when k < ky -> unsafeFindMax def
            | Tip(ky, y) -> Some(ky, y)
            | Nil -> unsafeFindMax def
        match t with
        | Bin(_, m, l, r) when m < 0 -> if k >= 0 then go r l else go Nil r
        | _ -> go Nil t

    ///O(log n). Find smallest key greater or equal to the given one and return the corresponding (key, value) pair Credit: Haskell.org
    let tryFindGE k t =
        let rec go def =
            function
            | Bin(p, m, l, r) when nomatch k p m -> if k < p then unsafeFindMin l else unsafeFindMin def
            | Bin(p, m, l, r) when zero k m -> go r l
            | Bin(p, m, l, r) -> go def r
            | Tip(ky, y) when k > ky -> unsafeFindMin def
            | Tip(ky, y) -> Some(ky, y)
            | Nil -> unsafeFindMin def
        match t with
        | Bin(_, m, l, r) when m < 0 -> if k >= 0 then go Nil l else go l r
        | _ -> go Nil t

    ///O(1). The empty map. Credit: Haskell.org
    let empty = Nil

    ///O(1). A map of one element. Credit: Haskell.org
    let inline singleton k x = Tip(k, x)

    ///O(min(n,W)). Insert a new key/value pair in the map. If the key is already present in the map, the associated value is replaced with the supplied value, i.e. insert is equivalent to insertWith const. Credit: Haskell.org
    let rec insert k x t =
        match t with
        | Bin(p, m, l, r) when nomatch k p m -> join k (Tip(k, x)) p t
        | Bin(p, m, l, r) when zero k m -> Bin(p, m, insert k x l, r)
        | Bin(p, m, l, r) -> Bin(p, m, l, insert k x r)
        | Tip(ky, _) when k = ky -> Tip(k, x)
        | Tip(ky, _) -> join k (Tip(k, x)) ky t
        | Nil -> Tip(k, x)

    ///O(min(n,W)). Insert with a combining function. insertWithKey f key value mp will insert the pair (key, value) into mp if key does not exist in the map. If the key does exist, the function will insert f key new_value old_value. Credit: Haskell.org
    let rec insertWithKey f k x t =
        match t with
        | Bin(p, m, l, r) when nomatch k p m -> join k (Tip(k, x)) p t
        | Bin(p, m, l, r) when zero k m -> Bin(p, m, insertWithKey f k x l, r)
        | Bin(p, m, l, r) -> Bin(p, m, l, insertWithKey f k x r)
        | Tip(ky, y) when k = ky -> Tip(k, f k x y)
        | Tip(ky, _) -> join k (Tip(k, x)) ky t
        | Nil -> Tip(k, x)

    ///O(min(n,W)). Insert with a combining function. insertWith f key value mp will insert the pair (key, value) into mp if key does not exist in the map. If the key does exist, the function will insert f new_value old_value. Credit: Haskell.org
    let insertWith f k x t = insertWithKey (fun _ x' y' -> f x' y') k x t

    ///O(min(n,W)). The expression (insertLookupWithKey f k x map) is a pair where the first element is equal to (lookup k map) and the second element equal to (insertWithKey f k x map). Credit: Haskell.org
    let rec insertTryFindWithKey f k x t =
        match t with
        | Bin(p, m, l, r) when nomatch k p m -> (None, join k (Tip(k, x)) p t)
        | Bin(p, m, l, r) when zero k m ->
            let found, l = insertTryFindWithKey f k x l
            (found, Bin(p, m, l, r))
        | Bin(p, m, l, r) ->
            let found, r = insertTryFindWithKey f k x r
            (found, Bin(p, m, l, r))
        | Tip(ky, y) when k = ky -> (Some y, Tip(k, f k x y))
        | Tip(ky, _) -> (None, join k (Tip(k, x)) ky t)
        | Nil -> (None, Tip(k, x))

    ///O(min(n,W)). Delete a key and its value from the map. When the key is not a member of the map, the original map is returned. Credit: Haskell.org
    let rec delete k t =
        match t with
        | Bin(p, m, l, r) when nomatch k p m -> t
        | Bin(p, m, l, r) when zero k m -> bin p m (delete k l) r
        | Bin(p, m, l, r) -> bin p m l (delete k r)
        | Tip(ky, _) when k = ky -> Nil
        | Tip _ -> t
        | Nil -> Nil

    ///O(min(n,W)). The expression (update f k map) updates the value x at k (if it is in the map). If (f k x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y. Credit: Haskell.org
    let rec updateWithKey f k t =
        match t with
        | Bin(p, m, l, r) when nomatch k p m -> t
        | Bin(p, m, l, r) when zero k m -> bin p m (updateWithKey f k l) r
        | Bin(p, m, l, r) -> bin p m l (updateWithKey f k r)
        | Tip(ky, y) when k = ky ->
            match f k y with
            | Some y -> Tip(ky, y)
            | None -> Nil
        | Tip _ -> t
        | Nil -> Nil

    ///O(min(n,W)). The expression (update f k map) updates the value x at k (if it is in the map). If (f x) is Nothing, the element is deleted. If it is (Just y), the key k is bound to the new value y. Credit: Haskell.org
    let update f k m = updateWithKey (fun _ x -> f x) k m

    ///O(min(n,W)). Adjust a value at a specific key. When the key is not a member of the map, the original map is returned. Credit: Haskell.org
    let adjustWithKey f k m = updateWithKey (fun k' x -> Some (f k' x)) k m

    ///O(min(n,W)). Adjust a value at a specific key. When the key is not a member of the map, the original map is returned. Credit: Haskell.org
    let adjust f k m = adjustWithKey (fun _ x -> f x) k m

    ///O(min(n,W)). Lookup and update. Credit: Haskell.org
    let rec updateTryFindWithKey f k t =
        match t with
        | Bin(p, m, l, r) when nomatch k p m -> (None, t)
        | Bin(p, m, l, r) when zero k m ->
            let (found, l) = updateTryFindWithKey f k l
            (found, bin p m l r)
        | Bin(p, m, l, r) ->
            let (found, r) = updateTryFindWithKey f k r
            (found, bin p m l r)
        | Tip(ky, y) when k = ky ->
            match f k y with
            | Some y' -> (Some y, Tip(ky, y'))
            | None -> (Some y, Nil)
        | Tip(ky, _) -> (None, t)
        | Nil -> (None, Nil)

    ///O(log n). The expression (alter f k map) alters the value x at k, or absence thereof. alter can be used to insert, delete, or update a value in an IntMap. Credit: Haskell.org
    let rec alter f k t =
        match t with
        | Bin(p, m, l, r) when nomatch k p m ->
            match f None with
            | None -> t
            | Some x -> join k (Tip(k, x)) p t
        | Bin(p, m, l, r) when zero k m -> bin p m (alter f k l) r
        | Bin(p, m, l, r) -> bin p m l (alter f k r)
        | Tip(ky, y) when k = ky ->
            match f (Some y) with
            | Some x -> Tip(ky, x)
            | None -> Nil
        | Tip(ky, y) ->
            match f None with
            | Some x -> join k (Tip(k, x)) ky t
            | None -> Tip(ky, y)
        | Nil ->
            match f None with
            | Some x -> Tip(k, x)
            | None -> Nil

    let inline private mergeWithKey' bin' f g1 g2 =

        let inline maybe_join p1 t1 p2 t2  =
            match t1, t2 with
            | Nil, t2 -> t2
            | t1, Nil -> t1
            | _ ->  join p1 t1 p2 t2
     
        let rec merge1 p1 m1 t1 l1 r1 p2 m2 t2 =
            if nomatch p2 p1 m1 then maybe_join p1 (g1 t1) p2 (g2 t2)
            elif zero p2 m1 then bin' p1 m1 (go l1 t2) (g1 r1)
            else bin' p1 m1 (g1 l1) (go r1 t2)

        and merge2 p1 m1 t1 p2 m2 t2 l2 r2 =
            if nomatch p1 p2 m2 then maybe_join p1 (g1 t1) p2 (g2 t2)
            elif zero p1 m2 then bin' p2 m2 (go t1 l2) (g2 r2)
            else bin' p2 m2 (g2 l2) (go t1 r2)

        and go t1 t2 =
            match t1, t2 with
            | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) when shorter m1 m2 -> merge1 p1 m1 t1 l1 r1 p2 m2 t2
            | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) when shorter m2 m1 -> merge2 p1 m1 t1 p2 m2 t2 l2 r2
            | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) when p1 = p2 -> bin' p1 m1 (go l1 l2) (go r1 r2)
            | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) -> maybe_join p1 (g1 t1) p2 (g2 t2)
            | Bin(_, _, _, _), Tip( k2', _) ->
                let rec merge t2 k2 t1 =
                    match t1 with
                    | Bin(p1, m1, l1, r1) when nomatch k2 p1 m1 -> maybe_join p1 (g1 t1) k2 (g2 t2)
                    | Bin(p1, m1, l1, r1) when zero k2 m1 -> bin' p1 m1 (merge t2 k2 l1) (g1 r1)
                    | Bin(p1, m1, l1, r1) -> bin' p1 m1 (g1 l1) (merge t2 k2 r1)
                    | Tip(k1, _) when k1 = k2 -> f t1 t2
                    | Tip(k1, _) -> maybe_join k1 (g1 t1) k2 (g2 t2)
                    | Nil -> g2 t2
                merge t2 k2' t1
            | Bin(_, _, _, _), Nil -> g1 t1
            | Tip(k1', _), t2' -> 
                let rec merge t1 k1 t2 =
                    match t2 with
                    | Bin(p2, m2, l2, r2) when nomatch k1 p2 m2 -> maybe_join k1 (g1 t1) p2 (g2 t2)
                    | Bin(p2, m2, l2, r2) when zero k1 m2 -> bin' p2 m2 (merge t1 k1 l2) (g2 r2)
                    | Bin(p2, m2, l2, r2) -> bin' p2 m2 (g2 l2) (merge t1 k1 r2)
                    | Tip(k2, _) when k1 = k2 -> f t1 t2
                    | Tip(k2, _) -> maybe_join k1 (g1 t1) k2 (g2 t2)
                    | Nil -> g1 t1
                merge t1 k1' t2'
            | Nil, t2 -> g2 t2
        go

    ///Refer to Haskell documentation. Unexpected code growth or corruption of the data structure can occure from wrong use. Credit: Haskell.org
    let mergeWithKey f g1 g2 =
        let combine =
            fun (Tip(k1, x1)) (Tip(_, x2)) ->
                match f k1 x1 x2 with
                | None -> Nil
                | Some x -> Tip(k1, x)
        mergeWithKey' bin combine g1 g2

    let append m1 m2 = mergeWithKey' (fun x y m1' m2' -> Bin(x, y, m1', m2')) konst id id m1 m2

    let appendWithKey f m1 m2 =
        mergeWithKey' (fun x y m1' m2' -> Bin(x, y, m1', m2')) (fun (Tip(k1, x1)) (Tip(_, x2)) -> Tip(k1, f k1 x1 x2)) id id m1 m2

    let appendWith f m1 m2 = appendWithKey (fun _ x y -> f x y) m1 m2

    let concat xs = List.fold append empty xs

    let concatWith f xs = List.fold (appendWith f) empty xs

    ///O(n+m). Difference between two maps (based on keys). Credit: Haskell.org
    let difference m1 m2 = mergeWithKey (fun _ _ _ -> None) id (konst Nil) m1 m2

    ///O(n+m). Difference with a combining function. When two equal keys are encountered, the combining function is applied to the key and both values. If it returns Nothing, the element is discarded (proper set difference). If it returns (Just y), the element is updated with a new value y. Credit: Haskell.org
    let differenceWithKey f m1 m2 = mergeWithKey f id (konst Nil) m1 m2

    ///O(n+m). Difference with a combining function. Credit: Haskell.org
    let differenceWith f m1 m2 = differenceWithKey (fun _ x y -> f x y) m1 m2

    ///O(n+m). The (left-biased) intersection of two maps (based on keys). Credit: Haskell.org
    let intersection m1 m2 = mergeWithKey' bin konst (konst Nil) (konst Nil) m1 m2

    ///O(n+m). The intersection with a combining function. Credit: Haskell.org
    let intersectionWithKey f m1 m2 =
        mergeWithKey' bin (fun (Tip(k1, x1)) (Tip(_, x2)) -> Tip(k1, f k1 x1 x2)) (konst Nil) (konst Nil) m1 m2

    ///O(n+m). The intersection with a combining function. Credit: Haskell.org
    let intersectionWith f m1 m2 = intersectionWithKey (fun _ x y -> f x y) m1 m2

    ///O(log n). Update the value at the minimal key. Credit: Haskell.org
    let updateMinWithKey f t =
        let rec go f =
            function
            | Bin(p, m, l, r) -> bin p m (go f l) r
            | Tip(k, y) ->
                match f k y with
                | Some y -> Tip(k, y)
                | None -> Nil
            | Nil -> failwith "updateMinWithKey Nil"
        match t with
        | Bin(p, m, l, r) when m < 0 -> bin p m l (go f r)
        | _ -> go f t

    ///O(log n). Update the value at the maximal key. Credit: Haskell.org
    let updateMaxWithKey f t =
        let rec go f =
            function
            | Bin(p, m, l, r) -> bin p m l (go f r)
            | Tip(k, y) ->
                match f k y with
                | Some y -> Tip(k, y)
                | None -> Nil
            | Nil -> failwith "updateMaxWithKey Nil"
        match t with
        | Bin(p, m, l, r) when m < 0 -> bin p m (go f l) r
        | _ -> go f t

    ///O(log n). Retrieves the maximal (key,value) couple of the map, and the map stripped from that element. fails (in the monad) when passed an empty map. Credit: Haskell.org
    let maxViewWithKey t =
        let rec go =
            function
            | Bin(p, m, l, r) -> let (result, r) = go r in (result, bin p m l r)
            | Tip(k, y) -> ((k, y), Nil)
            | Nil -> failwith "maxViewWithKey Nil"
        match t with
        | Nil -> None
        | Bin(p, m, l, r) when m < 0 -> let (result, l) = go l in Some(result, bin p m l r)
        | _ -> Some(go t)

    ///O(log n). Retrieves the minimal (key,value) couple of the map, and the map stripped from that element. fails (in the monad) when passed an empty map. Credit: Haskell.org
    let minViewWithKey t =
        let rec go =
            function
            | Bin(p, m, l, r) -> let (result, l) = go l in (result, bin p m l r)
            | Tip(k, y) -> ((k,y), Nil)
            | Nil -> failwith "minViewWithKey Nil"
        match t with
        | Nil -> None
        | Bin(p, m, l, r) when m < 0 -> let (result, r) = go r in Some(result, bin p m l r)
        | _ -> Some(go t)

    ///O(log n). Update the value at the maximal key. Credit: Haskell.org
    let updateMax f = updateMaxWithKey (konst f)

    ///O(log n). Update the value at the minimal key. Credit: Haskell.org
    let updateMin f = updateMinWithKey (konst f)

    let private first f (x, y) = (f x, y)

    ///O(min(n,W)). Retrieves the maximal key of the map, and the map stripped of that element, or Nothing if passed an empty map. Credit: Haskell.org
    let maxView t = Option.map (first snd) (maxViewWithKey t)

    ///O(min(n,W)). Retrieves the minimal key of the map, and the map stripped of that element, or Nothing if passed an empty map. Credit: Haskell.org
    let minView t = Option.map (first snd) (minViewWithKey t)

    ///O(log n). Retrieves the maximal key of the map, and the map stripped from that element. Credit: Haskell.org
    let deleteFindMax t = Option.getOrElseF (fun _ -> failwith "deleteFindMax: empty map has no maximal element") << maxViewWithKey <| t

    ///O(log n). Retrieves the minimal key of the map, and the map stripped from that element. Credit: Haskell.org
    let deleteFindMin t = Option.getOrElseF (fun _ -> failwith "deleteFindMin: empty map has no minimal element") << minViewWithKey <| t

    ///O(log n). The minimal key of the map. Credit: Haskell.org
    let findMin t =
        let rec go =
            function
            | Tip(k, v) -> (k, v)
            | Bin(_, _, l, _) -> go l
            | Nil -> failwith "findMin Nil"
        match t with
        | Nil -> failwith "findMin: empty map has no minimal element"
        | Tip(k, v) -> (k, v)
        | Bin(_, m, l, r) when m < 0 -> go r
        | Bin(_, m, l, r) -> go l

    ///O(log n). The maximal key of the map. Credit: Haskell.org
    let findMax t =
        let rec go =
            function
            | Tip(k, v) -> (k, v)
            | Bin(_, _, _, r) -> go r
            | Nil -> failwith "findMax Nil"
        match t with
        | Nil -> failwith "findMax: empty map has no maximal element"
        | Tip(k, v) -> (k, v)
        | Bin(_, m, l, r) when m < 0 -> go l
        | Bin(_, m, l, r) -> go r

    ///O(log n). Delete the minimal key. Credit: Haskell.org
    let deleteMin t = Option.getOrElseWith Nil snd << minView <| t

    ///O(log n). Delete the maximal key. Credit: Haskell.org
    let deleteMax t = Option.getOrElseWith Nil snd << maxView <| t

    ///O(n). Map a function over all values in the map. Credit: Haskell.org
    let rec mapWithKey f =
        function
        | Bin(p, m, l, r) -> Bin(p, m, mapWithKey f l, mapWithKey f r)
        | Tip(k, x) -> Tip(k, f k x)
        | Nil -> Nil

    ///O(n). Map a function over all values in the map. Credit: Haskell.org
    let rec map f =
        function
        | Bin(p, m, l, r) -> Bin(p, m, map f l, map f r)
        | Tip(k, x) -> Tip(k, f x)
        | Nil -> Nil

    let rec private mapAccumL f a =
        function
        | Bin(p, m, l, r) ->
            let (a1,l) = mapAccumL f a l
            let (a2,r) = mapAccumL f a1 r
            (a2, Bin(p, m, l, r))
        | Tip(k, x) -> let (a,x) = f a k x in (a,Tip(k, x))
        | Nil -> (a, Nil)

    ///O(n). The function mapAccum threads an accumulating argument through the map in ascending order of keys. Credit: Haskell.org
    let mapAccumWithKey f a t = mapAccumL f a t

    ///O(n). The function mapAccumWithKey threads an accumulating argument through the map in ascending order of keys. Credit: Haskell.org
    let mapAccum f = mapAccumWithKey (fun a' _ x -> f a' x)

    ///O(n). Filter all keys/values that satisfy some predicate. Credit: Haskell.org
    let rec filterWithKey predicate =
        function
        | Bin(p, m, l, r) -> bin p m (filterWithKey predicate l) (filterWithKey predicate r)
        | Tip(k, x) when predicate k x -> Tip(k, x)
        | Tip _ -> Nil
        | Nil -> Nil

    ///O(n). Filter all values that satisfy some predicate. Credit: Haskell.org
    let filter p m = filterWithKey (fun _ x -> p x) m

    ///O(n). partition the map according to some predicate. The first map contains all elements that satisfy the predicate, the second all elements that fail the predicate. See also split. Credit: Haskell.org
    let rec partitionWithKey predicate t =
        match t with
        | Bin(p, m, l, r)  ->
            let (l1, l2) = partitionWithKey predicate l
            let (r1, r2) = partitionWithKey predicate r
            (bin p m l1 r1, bin p m l2 r2)
        | Tip(k, x) when predicate k x -> (t, Nil)
        | Tip _-> (Nil, t)
        | Nil -> (Nil, Nil)

    ///O(n). partition the map according to some predicate. The first map contains all elements that satisfy the predicate, the second all elements that fail the predicate. See also split. Credit: Haskell.org
    let partition p m = partitionWithKey (fun _ x -> p x) m

    ///O(n). Map keys/values and collect the Just results. Credit: Haskell.org
    let rec mapOptionWithKey f =
        function
        | Bin(p, m, l, r) -> bin p m (mapOptionWithKey f l) (mapOptionWithKey f r)
        | Tip(k, x) ->
            match f k x with
            | Some y -> Tip(k, y)
            | None -> Nil
        | Nil -> Nil

    ///O(n). Map values and collect the Just results. Credit: Haskell.org
    let mapOption f = mapOptionWithKey (fun _ x -> f x)

    ///O(n). Map keys/values and separate the Left and Right results. Credit: Haskell.org
    let rec mapChoiceWithKey f =
        function
        | Bin(p, m, l, r) ->
            let (l1, l2) = mapChoiceWithKey f l
            let (r1, r2) = mapChoiceWithKey f r
            (bin p m l1 r1, bin p m l2 r2)
        | Tip(k, x) ->
            match f k x with
            | Choice1Of2 y  -> (Tip(k, y), Nil)
            | Choice2Of2 z -> (Nil, Tip(k, z))
        | Nil -> (Nil, Nil)

    ///O(n). Map values and separate the Left and Right results. Credit: Haskell.org
    let mapChoice f = mapChoiceWithKey (fun _ x -> f x)

    ///O(log n). The expression (split k map) is a pair (map1,map2) where all keys in map1 are lower than k and all keys in map2 larger than k. Any key equal to k is found in neither map1 nor map2. Credit: Haskell.org
    let split k t =
        let rec go k t =
            match t with
            | Bin(p, m, l, r) when nomatch k p m -> if k > p then (t, Nil) else (Nil, t)
            | Bin(p, m, l, r) when zero k m ->
                let (lt, gt) = go k l
                (lt, append gt r)
            | Bin(p, m, l, r) ->
                let (lt, gt) = go k r
                (append l lt, gt)
            | Tip(ky, _) when k > ky -> (t, Nil)
            | Tip(ky, _) when k < ky -> (Nil, t)
            | Tip(ky, _) -> (Nil, Nil)
            | Nil -> (Nil, Nil)
        match t with
        | Bin(_, m, l, r) when  m < 0 ->
            if k >= 0 // handle negative numbers.
                then let (lt, gt) = go k l in let lt = append r lt in (lt, gt)
            else let (lt, gt) = go k r in let gt = append gt l in (lt, gt)
        | _ -> go k t

    ///O(log n). Performs a split but also returns whether the pivot key was found in the original map. Credit: Haskell.org
    let splitTryFind k t =
        let rec go k t =
            match t with
            | Bin(p, m, l, r) when nomatch k p m -> if k > p then (t, None, Nil) else (Nil, None, t)
            | Bin(p, m, l, r) when zero k m ->
                let (lt, fnd, gt) = go k l
                let gt = append gt r
                (lt, fnd, gt)
            | Bin(p, m, l, r) ->
                let (lt, fnd, gt) = go k r
                let lt = append l lt
                (lt, fnd, gt)
            | Tip(ky, y) when k > ky -> (t, None, Nil)
            | Tip(ky, y) when k < ky -> (Nil, None, t)
            | Tip(ky, y) -> (Nil, Some y, Nil)
            | Nil -> (Nil, None, Nil)
        match t with
        | Bin(_, m, l, r) when  m < 0 ->
            if k >= 0 // handle negative numbers.
                then let (lt, fnd, gt) = go k l in let lt = append r lt in (lt, fnd, gt)
            else let (lt, fnd, gt) = go k r in let gt = append gt l in (lt, fnd, gt)
        | _ -> go k t

    ///O(n). FoldBack the values in the map, such that fold f z == Prelude.foldr f z . elems. Credit: Haskell.org
    let foldBack f z =
        let rec go z =
            function
            | Nil -> z
            | Tip(_, x) -> f x z
            | Bin(_, _, l, r) -> go (go z r) l
        fun t ->
            match t with
            | Bin(_, m, l, r) when m < 0 -> go (go z l) r  // put negative numbers before.
            | Bin(_, m, l, r) -> go (go z r) l
            | _ -> go z t

    ///O(n). Fold the values in the map, such that fold f z == Prelude.foldr f z . elems. Credit: Haskell.org
    let fold f z =
        let rec go z =
            function
            | Nil -> z
            | Tip(_, x) -> f z x
            | Bin(_, _, l, r) -> go (go z l) r
        fun t ->
            match t with
            | Bin(_, m, l, r) when m < 0 -> go (go z r) l  // put negative numbers before.
            | Bin(_, m, l, r) -> go (go z l) r
            | _ -> go z t

    ///O(n). FoldBack the keys and values in the map, such that foldWithKey f z == Prelude.foldr (uncurry f) z . toAscList. Credit: Haskell.org
    let inline foldBackWithKey f z = fun (t: _ IntMap) -> t.FoldBackWithKey f z

    ///O(n). Fold the keys and values in the map, such that foldWithKey f z == Prelude.foldr (uncurry f) z . toAscList. Credit: Haskell.org
    let foldWithKey f z =
        let rec go z =
            function
            | Nil -> z
            | Tip(kx, x) -> f z kx x
            | Bin(_, _, l, r) -> go (go z l) r
        fun t ->
            match t with
            | Bin(_, m, l, r) when m < 0 -> go (go z r) l  // put negative numbers before.
            | Bin(_, m, l, r) -> go (go z l) r
            | _ -> go z t
    
    ///O(n). Return all elements of the map in the ascending order of their keys. Credit: Haskell.org
    let values m = foldBack List.cons [] m

    ///O(n). Return all keys of the map in ascending order. Credit: Haskell.org
    let keys m = foldBackWithKey (fun k _ ks -> k :: ks) [] m

    ///O(n). Convert the map to a list of key/value pairs. Credit: Haskell.org
    let inline toList (m: _ IntMap) = m.ToList()

    ///O(n). Convert the map to a seq of key/value pairs. Credit: Haskell.org
    let toSeq m = m |> toList |> List.toSeq

    ///O(n). Convert the map to an array of key/value pairs. Credit: Haskell.org
    let toArray m = m |> toList |> List.toArray

    ///O(n*min(n,W)). Create a map from a list of key/value pairs. Credit: Haskell.org
    let ofList xs =
        let ins t (k, x) = insert k x t
        List.fold ins empty xs

    ///O(n*min(n,W)). Build a map from a list of key/value pairs with a combining function. See also fromAscListWithKey'. Credit: Haskell.org
    let ofListWithKey f xs =
        let ins t (k, x) = insertWithKey f k x t
        List.fold ins empty xs

    ///O(n*min(n,W)). Create a map from a list of key/value pairs with a combining function. See also fromAscListWith. Credit: Haskell.org
    let ofListWith f xs = ofListWithKey (fun _ x y -> f x y) xs

    ///O(n*min(n,W)). Create a map from a seq of key/value pairs. Credit: Haskell.org
    let ofSeq xs = xs |> List.ofSeq |> ofList

    ///O(n*min(n,W)). Build a map from a seq of key/value pairs with a combining function. See also fromAscListWithKey'. Credit: Haskell.org
    let ofSeqWithKey f xs = xs |> List.ofSeq |> ofListWithKey f

    ///O(n*min(n,W)). Create a map from a seq of key/value pairs with a combining function. See also fromAscListWith. Credit: Haskell.org
    let ofSeqWith f xs = xs |> List.ofSeq |> ofListWith f

    ///O(n*min(n,W)). Create a map from an array of key/value pairs. Credit: Haskell.org
    let ofArray xs = xs |> List.ofArray |> ofList

    ///O(n*min(n,W)). Build a map from an array of key/value pairs with a combining function. See also fromAscListWithKey'. Credit: Haskell.org
    let ofArrayWithKey f xs = xs |> List.ofArray |> ofListWithKey f

    ///O(n*min(n,W)). Create a map from an array of key/value pairs with a combining function. See also fromAscListWith. Credit: Haskell.org
    let ofArrayWith f xs = xs |> List.ofArray |> ofListWith f

    ///O(n*min(n,W)). mapKeys f s is the map obtained by applying f to each key of s. The size of the result may be smaller if f maps two or more distinct keys to the same new key. In this case the value at the greatest of the original keys is retained. Credit: Haskell.org
    let mapKeys f = ofList << foldBackWithKey (fun k x xs -> (f k, x) :: xs) []

    ///O(n*log n). mapKeysWith c f s is the map obtained by applying f to each key of s. The size of the result may be smaller if f maps two or more distinct keys to the same new key. In this case the associated values will be combined using c. Credit: Haskell.org
    let mapKeysWith c f = ofListWith c << foldBackWithKey (fun k x xs -> (f k, x) :: xs) []

    ///O(n+m). The expression (isSubmapOfBy f m1 m2) returns True if all keys in m1 are in m2, and when f returns True when applied to their respective values. Credit: Haskell.org
    let rec isSubmapOfBy predicate t1 t2 =
      match t1, t2 with
      | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) when shorter m1 m2 -> false
      | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) when shorter m2 m1 ->
          match' p1 p2 m2 &&
              (if zero p1 m2 then isSubmapOfBy predicate t1 l2
                  else isSubmapOfBy predicate t1 r2)
      | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) ->
          (p1 = p2) && isSubmapOfBy predicate l1 l2 && isSubmapOfBy predicate r1 r2
      | Bin _, _ -> false
      | Tip(k, x), t ->
          match tryFind k t with
          | Some y  -> predicate x y
          | None -> false
      | Nil, _ -> true

    ///O(n+m). Is this a submap? Defined as (isSubmapOf = isSubmapOfBy (==)). Credit: Haskell.org
    let isSubmapOf m1 m2 = isSubmapOfBy (=) m1 m2

    type private Ordering =
        | GT
        | LT
        | EQ

    let rec private submapCmp predicate t1 t2 =

        let submapCmpLt p1 r1 t1 p2 m2 l2 r2  =
            if nomatch p1 p2 m2 then GT
            elif zero p1 m2 then submapCmp predicate t1 l2
            else submapCmp predicate t1 r2

        let submapCmpEq l1 r1 l2 r2 =
            match (submapCmp predicate l1 l2, submapCmp predicate r1 r2) with
            | (GT,_ ) -> GT
            | (_ ,GT) -> GT
            | (EQ,EQ) -> EQ
            | _ -> LT
        match t1, t2 with
        | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) when shorter m1 m2 -> GT
        | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) when shorter m2 m1 -> submapCmpLt p1 r1 t1 p2 m2 l2 r2
        | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) when p1 = p2 -> submapCmpEq l1 r1 l2 r2
        | Bin(p1, m1, l1, r1), Bin(p2, m2, l2, r2) -> GT  // disjoint
        | Bin _, _ -> GT
        | Tip(kx, x), Tip(ky, y) when (kx = ky) && predicate x y -> EQ
        | Tip(kx, x), Tip(ky, y) -> GT  // disjoint
        | Tip(k, x), t ->
            match tryFind k t with
            | Some y when predicate x y -> LT
            | _ -> GT // disjoint
        | Nil, Nil -> EQ
        | Nil, _ -> LT

    ///O(n+m). Is this a proper submap? (ie. a submap but not equal). The expression (isProperSubmapOfBy f m1 m2) returns True when m1 and m2 are not equal, all keys in m1 are in m2, and when f returns True when applied to their respective values.  Credit: Haskell.org
    let isProperSubmapOfBy predicate t1 t2 =
        match submapCmp predicate t1 t2 with
        | LT -> true
        | _ -> false

    ///O(n+m). Is this a proper submap? (ie. a submap but not equal). Defined as (isProperSubmapOf = isProperSubmapOfBy (==)). Credit: Haskell.org
    let isProperSubmapOf m1 m2 = isProperSubmapOfBy (=) m1 m2

    let monoid<'a> = 
        { new Monoid<'a IntMap>() with
            override x.Zero() = empty
            override x.Combine(a,b) = append a b }
