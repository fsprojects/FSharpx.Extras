namespace FSharpx.Tests

open Microsoft.FSharp.Collections
open NUnit.Framework

[<TestFixture>]
type public ResizeArrayTests() =
  
    [<Test>]
    member this.BasicTests() = 
        let ra = ResizeArray.ofList
        let (=?) a b = ResizeArray.toList a = b

        test "ra_exists2_a" <| ResizeArray.exists2 (=)
            (ra [1; 2; 3; 4; 5; 6])
            (ra [2; 3; 4; 5; 6; 6])

        test "exists2_b" <| not (ResizeArray.exists2 (=)
            (ra [1; 2; 3; 4; 5; 6])
            (ra [2; 3; 4; 5; 6; 7]))

        test "ra_findIndex_a"
            (ResizeArray.findIndex (fun i -> i >= 4) (ra [0..10]) = 4)

        test "ra_findIndex_b"
            (try ResizeArray.findIndex (fun i -> i >= 20) (ra [0..10]) |> ignore; false
             with _ -> true)
       
        test "ra_find_indexi_a"
            (ResizeArray.findIndexi (=) (ra [1; 2; 3; 3; 2; 1]) = 3)

        test "ra_find_indexi_b"
            (try ResizeArray.findIndexi (=) (ra [1..10]) |> ignore; false
             with _ -> true)

        test "ra_forall2_a"
            (ResizeArray.forall2 (=) (ra [1..10]) (ra [1..10]))

        test "ra_forall2_b" <| not
            (ResizeArray.forall2 (=) (ra [1;2;3;4;5]) (ra [1;2;3;0;5]))

        test "ra_isEmpty_a"
            (ResizeArray.isEmpty (ra []))

        test "ra_isEmpty_b" <| not
            (ResizeArray.isEmpty (ra [1; 2]))

        test "ra_mapi2"
            (ResizeArray.mapi2 (fun i j k -> i+j+k) (ra [1..10]) (ra [1..10]) =? [2..+3..29])

        test "ra_mapi2_b"
            (try ResizeArray.mapi2 (fun i j k -> i+j+k) (ra []) (ra [1..10]) |> ignore; false
             with _ -> true)

        let c = ref 0
        ResizeArray.iteri2 (fun i j k -> c := !c+i+j+k) (ra [1;2;3]) (ra [10;20;30])
        test "ra_iteri2" (!c = 6+60+3)

        test "ra_singleton"
            (ResizeArray.singleton 42 =? [42])

        test "ra_zip"
            (ResizeArray.zip (ra [1..10]) (ra [1..10]) =? [for i in 1..10 -> i, i])

        let unzip1, unzip2 = ResizeArray.unzip <| ra [for i in 1..10 -> i, i+1]
        test "ra_unzip" (unzip1 =? [1..10] && unzip2 =? [2..11])

        test "ra_reduce_left"
            (ResizeArray.reduce (+) (ra [2;2;2;2]) = 8)

        test "ra_reduce_right"
            (ResizeArray.reduceBack (+) (ra [2;2;2;2]) = 8)

        test "ra_fold2"
            (ResizeArray.fold2 (fun i j k -> i+j+k) 100 (ra [1;2;3]) (ra [1;2;3]) = 112)

        test "ra_fold2_b"
            (ResizeArray.fold2 (fun i j k -> i-j-k) 100 (ra [1;2;3]) (ra [1;2;3]) = 100-12)

        test "ra_foldBack2"
            (ResizeArray.foldBack2 (fun i j k -> i+j+k) (ra [1;2;3]) (ra [1;2;3]) 100 = 112)

        test "ra_foldBack2_b"
            (ResizeArray.foldBack2 (fun i j k -> k-i-j) (ra [1;2;3]) (ra [1;2;3]) 100 = 100-12)

        test "ra_scan"
            (ResizeArray.scan (+) 0 (ra [1..5]) =? [0; 1; 3; 6; 10; 15])

        test "ra_scanBack"
            (ResizeArray.scanBack (+) (ra [1..5]) 0 =? [15; 14; 12; 9; 5; 0])

        test "ra_tryfind_index"
            (ResizeArray.tryFindIndex (fun x -> x = 4) (ra [0..10]) = Some 4)

        test "ra_tryfind_index_b"
            (ResizeArray.tryFindIndex (fun x -> x = 42) (ra [0..10]) = None)

        test "ra_tryfind_indexi"
            (ResizeArray.tryFindIndexi (=) (ra [1;2;3;4;4;3;2;1]) = Some 4)

        test "ra_tryfind_indexi_b"
            (ResizeArray.tryFindIndexi (=) (ra [1..10]) = None)

        c := -1
        ResizeArray.iter (fun x -> incr c; test "ra_iter" (x = !c)) (ra [0..100])
        test "ra_iter" (!c = 100)

        test "ra_map"
            (ra [1..100] |> ResizeArray.map ((+) 1) =? [2..101])

        test "ra_mapi"
            (ra [0..100] |> ResizeArray.mapi (+) =? [0..+2..200])

        c := -1
        ResizeArray.iteri (fun i x -> incr c; test "ra_iteri" (x = !c && i = !c)) (ra [0..100])
        test "ra_iteri" (!c = 100)

        test "ra_exists"
            (ra [1..100] |> ResizeArray.exists ((=) 50))

        test "ra_exists b" <| not
            (ra [1..100] |> ResizeArray.exists ((=) 150))

        test "ra_forall"
            (ra [1..100] |> ResizeArray.forall (fun x -> x < 150))

        test "ra_forall b" <| not
            (ra [1..100] |> ResizeArray.forall (fun x -> x < 80))

        test "ra_find"
            (ra [1..100] |> ResizeArray.find (fun x -> x > 50) = 51)

        test "ra_find b"
            (try ra [1..100] |> ResizeArray.find (fun x -> x > 180) |> ignore; false
             with _ -> true)

        test "ra_first"
            (ra [1..100] |> ResizeArray.tryPick (fun x -> if x > 50 then Some (x*x) else None) = Some (51*51))

        test "ra_first b"
            (ra [1..100] |> ResizeArray.tryPick (fun x -> None) = None)
            
        test "ra_first c"
            (ra [] |> ResizeArray.tryPick (fun _ -> Some 42) = None)

        test "ra_tryfind"
            (ra [1..100] |> ResizeArray.tryFind (fun x -> x > 50) = Some 51)

        test "ra_tryfind b"
            (ra [1..100] |> ResizeArray.tryFind (fun x -> x > 180) = None)

        c := -1
        ResizeArray.iter2 (fun x y -> incr c; test "ra_iter2" (!c = x && !c = y)) (ra [0..100]) (ra [0..100])
        test "ra_iter2" (!c = 100)

        test "ra_map2"
            (ResizeArray.map2 (+) (ra [0..100]) (ra [0..100]) =? [0..+2..200])

        test "ra_choose"
            (ResizeArray.choose (fun x -> if x % 2 = 0 then Some (x/2) else None) (ra [0..100]) =? [0..50])

        test "ra_filter"
            (ResizeArray.filter (fun x -> x % 2 = 0) (ra [0..100]) =? [0..+2..100])

        test "ra_filter b"
            (ResizeArray.filter (fun x -> false) (ra [0..100]) =? [])

        test "ra_filter c"
            (ResizeArray.filter (fun x -> true) (ra [0..100]) =? [0..100])

        let p1, p2 = ResizeArray.partition (fun x -> x % 2 = 0) (ra [0..100])
        test "ra_partition"
            (p1 =? [0..+2..100] && p2 =? [1..+2..100])

        test "ra_rev"
            (ResizeArray.rev (ra [0..100]) =? [100..-1 ..0])

        test "ra_rev b"
            (ResizeArray.rev (ra [1]) =? [1])

        test "ra_rev c"
            (ResizeArray.rev (ra []) =? [])

        test "ra_rev d"
            (ResizeArray.rev (ra [1; 2]) =? [2; 1])

