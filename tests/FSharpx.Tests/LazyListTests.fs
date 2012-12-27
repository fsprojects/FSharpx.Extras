// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack.Unittests/LazyListTests.fs

namespace FSharpx.Tests

open FSharpx.Collections
open NUnit.Framework

#nowarn "40"

[<TestFixture>]
type public LazyListTests() =
  
    [<Test>]
    member this.BasicTests1() = 

        let l = LazyList.ofList [1;2;3] in
        let res = ref 2 in 
        for i in (LazyList.toSeq l) do res := !res + i done;
        check "test2398984: LazyList.toSeq" 8 !res;
        let res = ref 2 in 
        for i in LazyList.toSeq l do 
            res := !res + i 
        done;
        check "test2398984: LazyList.toSeq" 8 !res
  
    [<Test>]
    member this.BasicTests2() = 
        let l = LazyList.ofList [1;2;3] in
        let res = ref 2 in 
        Seq.iter (fun i -> res := !res + i) (LazyList.toSeq l) 
        check "test2398994: foreach, LazyList.toSeq" 8 !res

    [<Test>]
    member this.BasicTests3() = 

        test "se1" (LazyList.isEmpty LazyList.empty)
        test "se2" (not (LazyList.isEmpty (LazyList.cons 1 LazyList.empty)))
        test "se3" (not (LazyList.isEmpty (LazyList.repeat 1)))
        test "se4" (not (LazyList.isEmpty (LazyList.unfold (fun z -> Some (z,z+1)) 0)))

        test "seq1" (LazyList.head (LazyList.cons 1 LazyList.empty) = 1)
        test "seq2" (LazyList.head (LazyList.cons 1 (LazyList.cons 2 LazyList.empty)) = 1)
        test "seq3" (LazyList.head (LazyList.tail (LazyList.cons 1 (LazyList.cons 2 LazyList.empty))) = 2)

        test "tryHead empty" ((LazyList.tryHead LazyList.empty) = None)
        test "tryHead seq1" ((LazyList.tryHead (LazyList.cons 1 LazyList.empty)).Value = 1)
        test "tryHead seq2" ((LazyList.tryHead (LazyList.cons 1 (LazyList.cons 2 LazyList.empty))).Value = 1)

        test "tryTail empty" ((LazyList.tryTail LazyList.empty) = None)
        test "tryTail seq1" (LazyList.isEmpty (LazyList.tryTail (LazyList.cons 1 LazyList.empty)).Value)
        test "tryTail seq2" (LazyList.toList ((LazyList.tryTail (LazyList.cons 1 (LazyList.cons 2 LazyList.empty))).Value) = [2])
        test "tryTail seq3" (LazyList.toList ((LazyList.tryTail (LazyList.cons 1 (LazyList.cons 2 (LazyList.cons 3 LazyList.empty)))).Value) = [2;3])

        let nats = LazyList.unfold (fun z -> Some (z,z+1)) 0 
        test "take1" (LazyList.toList (LazyList.take 4 nats) = [0;1;2;3])
        test "drop1" (LazyList.head (LazyList.skip 4 nats) = 4)
        test "drop1" (LazyList.head (LazyList.skip 0 nats) = 0)

        test "tryTake empty" ((LazyList.tryTake 4 LazyList.empty) = None)
        test "tryTake0" (LazyList.isEmpty (LazyList.tryTake 0 LazyList.empty).Value)
        test "tryTake1" (LazyList.toList (LazyList.tryTake 1 nats).Value = [0])
        test "tryTake4" (LazyList.toList (LazyList.tryTake 4 nats).Value = [0;1;2;3])
        test "tryTake4 from list of 3" (LazyList.toList (LazyList.tryTake 4 (LazyList.tryTake 3 nats).Value).Value = [0;1;2])

        test "trySkip 0" (LazyList.head (LazyList.trySkip 0 nats).Value = 0)
        test "trySkip 4" (LazyList.head (LazyList.trySkip 4 nats).Value = 4)
        test "trySkip -1" ((LazyList.trySkip -1 nats) = None)
        test "trySkip 4 from list of 3" ((LazyList.trySkip 4 (LazyList.tryTake 3 nats).Value) = None)

        test "tryUncons empty" ((LazyList.tryUncons LazyList.empty) = None)
        let x, y = (LazyList.tryUncons (LazyList.take 1 nats)).Value
        //LazyList has [<NoEquality; NoComparison>], so we cannot compare to LazyList.empty
//        test "tryUncons 1" ((x, y) = (0, LazyList.empty<int>))
        test "tryUncons 1" ((x, (LazyList.isEmpty y)) = (0, true))
        let x2, y2 = (LazyList.tryUncons (LazyList.take 2 nats)).Value
        test "tryUncons 2" ((x2, (LazyList.toList y2)) = (0, [1]))
        let x3, y3 = (LazyList.tryUncons (LazyList.take 3 nats)).Value
        test "tryUncons 2" ((x3, (LazyList.toList y3)) = (0, [1;2]))

        let xa, ya = LazyList.uncons (LazyList.take 1 nats)
        test "uncons 1" ((xa, (LazyList.isEmpty ya)) = (0, true))
        let xa2, ya2 = LazyList.uncons (LazyList.take 2 nats)
        test "uncons 2" ((xa2, (LazyList.toList ya2)) = (0, [1]))
        let xa3, ya3 = LazyList.uncons (LazyList.take 3 nats)
        test "uncons 2" ((xa3, (LazyList.toList ya3)) = (0, [1;2]))

        let ll = LazyList.ofList [-5..-1]
        let expected = (15, [5;4;3;2;1])
        let x, y = LazyList.mapAccum (fun a b -> let c = abs b in (a+c,c)) 0 ll
        test "mapAccum" ((x, (LazyList.toList y)) = expected)

        test "repeat" (LazyList.toList (LazyList.take 4 (LazyList.repeat 1)) = [1;1;1;1])
        test "append" (LazyList.toList (LazyList.take 4 (LazyList.append (LazyList.cons 77 (LazyList.empty)) nats)) = [77;0;1;2])
        test "zip"  (LazyList.toList (LazyList.take 3 (LazyList.zip nats (LazyList.skip 6 nats))) = [0,6;1,7; 2,8])
        test "firstS" (LazyList.tryFind (fun x -> x>=8) nats = Some 8)
        test "firstN" (LazyList.tryFind (fun x -> x>=8) (LazyList.take 5 nats) = None)
        test "find S" (LazyList.find (fun x -> x>=8) nats = 8)
        test "find N" (let res =
                              try
                                LazyList.find (fun x -> x>=8) (LazyList.take 5 nats)
                              with
                                  Not_found -> 9999
                       res = 9999) (* testing for exception *)

        let rec diverge () = diverge ()
        test "consfA"       (LazyList.head (LazyList.consDelayed 1 diverge) = 1)
        test "consfB"       (let ss = LazyList.tail (LazyList.consDelayed 1 diverge) in true) (* testing for lazy divergence *)
        test "dropDiverge1" (let ss = LazyList.skip 1 (LazyList.consDelayed 1 diverge) in true) (* testing for lazy divergence *)
        test "dropDiverge0" (let ss = LazyList.skip 0 (LazyList.delayed (fun () -> failwith "errors")) in true) (* testing for lazy divergence *)
        test "takedrop" (LazyList.toList (LazyList.take 3 (LazyList.skip 4 nats)) = [4;5;6])

        test "filter" (LazyList.toList (LazyList.take 4 (LazyList.filter (fun x -> x % 2 = 0) nats))     = [0;2;4;6])
        test "map"    (LazyList.toList (LazyList.take 4 (LazyList.map    (fun x -> x+1) nats))             = [1;2;3;4])
        test "map2"   (LazyList.toList (LazyList.take 4 (LazyList.map2   (fun x y -> x*y) nats (LazyList.tail nats))) = [0*1;1*2;2*3;3*4])

        test "array"  (Array.toList (LazyList.toArray (LazyList.take 6 nats)) = LazyList.toList (LazyList.take 6 nats))
        test "array"  (LazyList.toList (LazyList.ofArray [|1;2;3;4|]) = LazyList.toList (LazyList.ofList [1;2;3;4]))

        // This checks that LazyList.map, LazyList.length etc. are tail recursive
        check "LazyList.length" (LazyList.ofSeq (Seq.init 100 (fun c -> c)) |> LazyList.length) 100
        check "LazyList.length" (LazyList.ofSeq (Seq.init 1000000 (fun c -> c)) |> LazyList.length) 1000000
        check "LazyList.length" (LazyList.ofSeq (Seq.init 0 (fun c -> c)) |> LazyList.length) 0
        check "LazyList.map" (LazyList.map (fun x -> x + 1) (LazyList.ofSeq (Seq.init 1000000 (fun c -> c))) |> Seq.length) 1000000
        check "LazyList.filter" (LazyList.filter (fun x -> x % 2 = 0) (LazyList.ofSeq (Seq.init 1000000 (fun c -> c))) |> Seq.length) 500000
        check "LazyList.iter" (let count = ref 0 in LazyList.iter (fun x -> incr count) (LazyList.ofSeq (Seq.init 0 (fun c -> c))); !count) 0
        check "LazyList.iter" (let count = ref 0 in LazyList.iter (fun x -> incr count) (LazyList.ofSeq (Seq.init 1000000 (fun c -> c))); !count) 1000000
        check "LazyList.toList" (LazyList.toList (LazyList.ofSeq (Seq.init 200000 (fun c -> c))) |> Seq.length) 200000
        check "LazyList.toArray" (LazyList.toArray (LazyList.ofSeq (Seq.init 200000 (fun c -> c))) |> Seq.length) 200000

        /// check exists on an infinite stream terminates
        check "IEnumerableTest.exists" (Seq.exists ((=) "a") (LazyList.repeat "a" |> LazyList.toSeq)) true
        /// check a succeeding 'exists' on a concat of an infinite number of finite streams terminates
        check "IEnumerableTest.exists" (Seq.exists ((=) "a") (Seq.concat (LazyList.repeat [| "a"; "b"|] |> LazyList.toSeq))) true
        /// check a succeeding 'exists' on a concat of an infinite number of infinite streams terminates
        check "IEnumerableTest.exists" (Seq.exists ((=) "a") (Seq.concat (LazyList.repeat (LazyList.repeat "a" |> LazyList.toSeq) |> LazyList.toSeq))) true
        /// check a failing for_all on an infinite stream terminates
        check "IEnumerableTest.exists" (Seq.forall ((=) "a" >> not) (LazyList.repeat "a" |> LazyList.toSeq)) false
        /// check a failing for_all on a concat of an infinite number of finite streams terminates
        check "IEnumerableTest.exists" (Seq.forall ((=) "a" >> not) (Seq.concat (LazyList.repeat [| "a"; "b"|] |> LazyList.toSeq))) false
        check "IEnumerableTest.append, infinite, infinite, then take" (Seq.take 2 (Seq.append (LazyList.repeat "a" |> LazyList.toSeq) (LazyList.repeat "b" |> LazyList.toSeq)) |> Seq.toList) [ "a"; "a" ]
        /// check exists on an infinite stream terminates
        check "IEnumerableTest.exists" (Seq.exists ((=) "a") (LazyList.repeat "a" |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce)) true
        check "<dispoal>" !numActiveEnumerators 0
        /// check a succeeding 'exists' on a concat of an infinite number of finite streams terminates
        check "IEnumerableTest.exists" (Seq.exists ((=) "a") (Seq.concat (LazyList.repeat [| "a"; "b"|] |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce))) true
        check "<dispoal>" !numActiveEnumerators 0
        /// check a succeeding 'exists' on a concat of an infinite number of infinite streams terminates
        check "IEnumerableTest.exists" (Seq.exists ((=) "a") (Seq.concat (LazyList.repeat (LazyList.repeat "a" |> LazyList.toSeq) |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce))) true
        check "<dispoal>" !numActiveEnumerators 0
        /// check a failing for_all on an infinite stream terminates
        check "IEnumerableTest.exists" (Seq.forall ((=) "a" >> not) (LazyList.repeat "a" |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce)) false
        check "<dispoal>" !numActiveEnumerators 0

        /// check a failing for_all on a concat of an infinite number of finite streams terminates
        check "<dispoal>" !numActiveEnumerators 0
        check "IEnumerableTest.exists" (Seq.forall ((=) "a" >> not) (Seq.concat (LazyList.repeat [| "a"; "b"|] |> LazyList.toSeq |> countEnumeratorsAndCheckedDisposedAtMostOnce))) false
        check "<dispoal>" !numActiveEnumerators 0
        check "IEnumerableTest.append, infinite, infinite, then take" (Seq.take 2 (Seq.append (LazyList.repeat "a" |> LazyList.toSeq) (LazyList.repeat "b" |> LazyList.toSeq)) |> countEnumeratorsAndCheckedDisposedAtMostOnceAtEnd |> Seq.toList) [ "a"; "a" ]
        check "<dispoal>" !numActiveEnumerators 0
    

    [<Test>]
    member this.PatternsTests() = 

        let matchTwo ll = 
            match ll with 
            | LazyList.Cons(h1,LazyList.Cons(h2,t)) -> printf "%O,%O\n" h1 h2
            | LazyList.Cons(h1,t) -> printf "%O\n" h1
            | LazyList.Nil() -> printf "empty!\n" 

        let rec pairReduce xs =
          match xs with
            | LazyList.Cons (x, LazyList.Cons (y,ys)) -> LazyList.consDelayed (x+y) (fun () -> pairReduce ys)
            | LazyList.Cons (x, LazyList.Nil)      -> LazyList.cons x LazyList.empty
            | LazyList.Nil                 -> LazyList.empty 

        let rec inf = LazyList.consDelayed 0 (fun () -> LazyList.map (fun x -> x + 1) inf)

        let ll = LazyList.ofList [1;2;3;4]
        check "we09wek" (sprintf "%A" (LazyList.toList (LazyList.take 10 (pairReduce inf)))) "[1; 5; 9; 13; 17; 21; 25; 29; 33; 37]"

        check "we09wek" (LazyList.scan (+) 0 (LazyList.ofList [1;2])  |> LazyList.toList)  [0;1;3]
        check "we09wek" (LazyList.scan (+) 0 (LazyList.ofList [])  |> LazyList.toList)  [0]