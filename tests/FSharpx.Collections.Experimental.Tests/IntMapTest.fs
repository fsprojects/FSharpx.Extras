module FSharpx.Collections.Experimental.Tests.IntMapTest

open FSharpx
open FSharpx.Collections.Experimental
open NUnit.Framework
open FsUnit

[<Test>]
let ``test isEmpty``() =
    IntMap.isEmpty IntMap.empty |> should be True
    (1, 'a') ||> IntMap.singleton |> IntMap.isEmpty |> should be False

[<Test>]
let ``test size``() =
    IntMap.size IntMap.empty |> should equal 0
    IntMap.singleton 1 'a' |> IntMap.size |> should equal 1
    IntMap.ofList [(1,'a'); (2,'b'); (3,'c')] |> IntMap.size |> should equal 3

[<Test>]
let ``test member``() =
    IntMap.exists 5 (IntMap.ofList [(5,'a'); (3,'b')]) |> should be True
    IntMap.exists 1 (IntMap.ofList [(5,'a'); (3,'b')]) |> should be False

[<Test>]
let ``test notMember``() =
    IntMap.notExists 5 (IntMap.ofList [(5,'a'); (3,'b')]) |> should be False
    IntMap.notExists 1 (IntMap.ofList [(5,'a'); (3,'b')]) |> should be True

[<Test>]
let ``test tryFind``() =
    let employeeDept = IntMap.ofList([(1,2); (3,1)])
    let deptCountry = IntMap.ofList([(1,1); (2,2)])
    let countryCurrency = IntMap.ofList([(1, 2); (2, 1)])
    let employeeCurrency name = Option.maybe {
        let! dept = IntMap.tryFind name employeeDept
        let! country = IntMap.tryFind dept deptCountry
        return! IntMap.tryFind country countryCurrency
    }
    employeeCurrency 1 |> should equal (Some 1)
    employeeCurrency 2 |> should equal None

[<Test>]
let ``test find``() =
    let employeeDept = IntMap.ofList([(1,1); (2,2)])
    let x = IntMap.find 2 employeeDept

    x |> should equal 2


[<Test>]
let ``test findWithDefault``() =
    IntMap.findWithDefault 'x' 1 (IntMap.ofList [(5,'a'); (3,'b')]) |> should equal 'x'
    IntMap.findWithDefault 'x' 5 (IntMap.ofList [(5,'a'); (3,'b')]) |> should equal 'a'

[<Test>]
let ``test tryFindLT``() =
    IntMap.tryFindLT 3 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal None
    IntMap.tryFindLT 4 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal (Some(3, 'a'))

[<Test>]
let ``test tryFindGT``() =
    IntMap.tryFindGT 4 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal (Some(5,'b'))
    IntMap.tryFindGT 5 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal None

[<Test>]
let ``test tryFindLE``() =
    IntMap.tryFindLE 2 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal None
    IntMap.tryFindLE 4 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal (Some(3,'a'))
    IntMap.tryFindLE 5 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal (Some(5,'b'))

[<Test>]
let ``test tryFindGE``() =
    IntMap.tryFindGE 3 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal (Some(3,'a'))
    IntMap.tryFindGE 4 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal (Some(5,'b'))
    IntMap.tryFindGE 6 (IntMap.ofList [(3,'a'); (5,'b')]) |> should equal None

// Construction

[<Test>]
let ``test empty``() =
    IntMap.empty |> should equal (IntMap.ofList [])
    IntMap.size IntMap.empty |> should equal 0

[<Test>]
let ``test singleton``() =
    IntMap.singleton 1 'a' |> should equal (IntMap.ofList [(1, 'a')])
    IntMap.size (IntMap.singleton 1 'a') |> should equal 1

[<Test>]
let ``test insert``() =
    IntMap.insert 5 'x' (IntMap.ofList [(5,'a'); (3,'b')]) |> should equal (IntMap.ofList [(3, 'b'); (5, 'x')])
    IntMap.insert 7 'x' (IntMap.ofList [(5,'a'); (3,'b')]) |> should equal (IntMap.ofList [(3, 'b'); (5, 'a'); (7, 'x')])
    IntMap.insert 5 'x' IntMap.empty |> should equal (IntMap.singleton 5 'x')

[<Test>]
let ``test insertWith``() =
    IntMap.insertWith (+) 5 "xxx" (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "xxxa")])
    IntMap.insertWith (+) 7 "xxx" (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "a"); (7, "xxx")])
    IntMap.insertWith (+) 5 "xxx" IntMap.empty |> should equal (IntMap.singleton 5 "xxx")

[<Test>]
let ``test insertWithKey``() =
    let f key new_value old_value = string key + ":" + new_value + "|" + old_value
    IntMap.insertWithKey f 5 "xxx" (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "5:xxx|a")])
    IntMap.insertWithKey f 7 "xxx" (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "a"); (7, "xxx")])
    IntMap.insertWithKey f 5 "xxx" IntMap.empty |> should equal (IntMap.singleton 5 "xxx")

[<Test>]
let ``test insertTryFindWithKey``() =
    let f key new_value old_value = string key + ":" + new_value + "|" + old_value
    IntMap.insertTryFindWithKey f 5 "xxx" (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (Some "a", IntMap.ofList [(3, "b"); (5, "5:xxx|a")])
    let fnd, map = IntMap.insertTryFindWithKey f 2 "xxx" (IntMap.ofList [(5,"a"); (3,"b")])
    fnd |> should equal None
    map |> should equal (IntMap.ofList [(2,"xxx");(3,"b");(5,"a")])
    let fnd, map = IntMap.insertTryFindWithKey f 7 "xxx" (IntMap.ofList [(5,"a"); (3,"b")])
    fnd |> should equal None
    map |> should equal (IntMap.ofList [(3, "b"); (5, "a"); (7, "xxx")])
    let fnd, map = IntMap.insertTryFindWithKey f 5 "xxx" IntMap.empty
    fnd |> should equal None
    map |> should equal (IntMap.singleton 5 "xxx")

// Delete/Update

[<Test>]
let ``test delete``() =
    IntMap.delete 5 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 3 "b")
    IntMap.delete 7 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "a")])
    IntMap.delete 5 IntMap.empty |> should equal IntMap.empty

[<Test>]
let ``test adjust``() =
    IntMap.adjust ((+) "new ") 5 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "new a")])
    IntMap.adjust ((+) "new ") 7 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "a")])
    IntMap.adjust ((+) "new ") 7 IntMap.empty |> should equal IntMap.empty

[<Test>]
let ``test adjustWithKey``() =
    let f key x = (string key) + ":new " + x
    IntMap.adjustWithKey f 5 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "5:new a")])
    IntMap.adjustWithKey f 7 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "a")])
    IntMap.adjustWithKey f 7 IntMap.empty |> should equal IntMap.empty

[<Test>]
let ``test update``() =
    let f x = if x = "a" then Some "new a" else None
    IntMap.update f 5 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "new a")])
    IntMap.update f 7 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "a")])
    IntMap.update f 3 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 5 "a")

[<Test>]
let ``test updateWithKey``() =
    let f k x = if x = "a" then Some ((string k) + ":new a") else None
    IntMap.updateWithKey f 5 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "5:new a")])
    IntMap.updateWithKey f 7 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "a")])
    IntMap.updateWithKey f 3 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 5 "a")

[<Test>]
let ``test updateTryFindWithKey``() =
    let f k x = if x = "a" then Some ((string k) + ":new a") else None
    IntMap.updateTryFindWithKey f 5 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (Some "a", IntMap.ofList [(3, "b"); (5, "5:new a")])
    let fnd, map = IntMap.updateTryFindWithKey f 7 (IntMap.ofList [(5,"a"); (3,"b")])
    fnd |> should equal None
    map |> should equal (IntMap.ofList [(3, "b"); (5, "a")])
    IntMap.updateTryFindWithKey f 3 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (Some "b", IntMap.singleton 5 "a")

[<Test>]
let ``test alter``() =
    let f _ = None
    let g _ = Some "c"
    IntMap.alter f 7 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "a")])
    IntMap.alter f 5 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 3 "b")
    IntMap.alter g 7 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "a"); (7, "c")])
    IntMap.alter g 5 (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "c")])

// Combine

[<Test>]
let ``test append``() =
    IntMap.append (IntMap.ofList [(5, "a"); (3, "b")]) (IntMap.ofList [(5, "A"); (7, "C")])
    |> should equal (IntMap.ofList [(3, "b"); (5, "a"); (7, "C")])

[<Test>]
let ``test appendWith``() =
    IntMap.appendWith (+) (IntMap.ofList [(5, "a"); (3, "b")]) (IntMap.ofList [(5, "A"); (7, "C")])
    |> should equal (IntMap.ofList [(3, "b"); (5, "aA"); (7, "C")])

[<Test>]
let ``test appendWithKey``() = 
    let f key left_value right_value = (string key) + ":" + left_value + "|" + right_value
    IntMap.appendWithKey f (IntMap.ofList [(5, "a"); (3, "b")]) (IntMap.ofList [(5, "A"); (7, "C")])
    |> should equal (IntMap.ofList [(3, "b"); (5, "5:a|A"); (7, "C")])

[<Test>]
let ``test concat``() =
    IntMap.concat [IntMap.ofList [(5, "a"); (3, "b")]; IntMap.ofList [(5, "A"); (7, "C")]; IntMap.ofList [(5, "A3"); (3, "B3")]]
    |> should equal (IntMap.ofList [(3, "b"); (5, "a"); (7, "C")])
    IntMap.concat [IntMap.ofList [(5, "A3"); (3, "B3")]; IntMap.ofList [(5, "A"); (7, "C")]; IntMap.ofList [(5, "a"); (3, "b")]]
    |> should equal (IntMap.ofList [(3, "B3"); (5, "A3"); (7, "C")])

[<Test>]
let ``test concatWith``() =
    IntMap.concatWith (+) [IntMap.ofList [(5, "a"); (3, "b")]; IntMap.ofList [(5, "A"); (7, "C")]; IntMap.ofList [(5, "A3"); (3, "B3")]]
    |> should equal (IntMap.ofList [(3, "bB3"); (5, "aAA3"); (7, "C")])

[<Test>]
let ``test difference``() =
    IntMap.difference (IntMap.ofList [(5, "a"); (3, "b")]) (IntMap.ofList [(5, "A"); (7, "C")])
    |> should equal (IntMap.singleton 3 "b")

[<Test>]
let ``test differenceWith``() =
    let f al ar = if al = "b" then Some (al + ":" + ar) else None
    IntMap.differenceWith f (IntMap.ofList [(5, "a"); (3, "b")]) (IntMap.ofList [(5, "A"); (3, "B"); (7, "C")])
    |> should equal (IntMap.singleton 3 "b:B")

[<Test>]
let ``test differenceWithKey``() =
    let f k al ar = if al = "b" then Some ((string k) + ":" + al + "|" + ar) else None
    IntMap.differenceWithKey f (IntMap.ofList [(5, "a"); (3, "b")]) (IntMap.ofList [(5, "A"); (3, "B"); (10, "C")])
    |> should equal (IntMap.singleton 3 "3:b|B")

[<Test>]
let ``test intersection``() =
    IntMap.intersection (IntMap.ofList [(5, "a"); (3, "b")]) (IntMap.ofList [(5, "A"); (7, "C")])
    |> should equal (IntMap.singleton 5 "a")

[<Test>]
let ``test intersectionWith``() =
    IntMap.intersectionWith (+) (IntMap.ofList [(5, "a"); (3, "b")]) (IntMap.ofList [(5, "A"); (7, "C")])
    |> should equal (IntMap.singleton 5 "aA")

[<Test>]
let ``test intersectionWithKey``() =
    let f k al ar = (string k) + ":" + al + "|" + ar
    IntMap.intersectionWithKey f (IntMap.ofList [(5, "a"); (3, "b")]) (IntMap.ofList [(5, "A"); (7, "C")])
    |> should equal (IntMap.singleton 5 "5:a|A")

// Traversal

[<Test>]
let ``test map``() =
    IntMap.map (flip (+) "x") (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "bx"); (5, "ax")])

[<Test>]
let ``test mapWithKey``() =
    let f key x = (string key) + ":" + x
    IntMap.mapWithKey f (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "3:b"); (5, "5:a")])

[<Test>]
let ``test mapAccum``() =
    let f a b = (a + b, b + "X")
    IntMap.mapAccum f "Everything: " (IntMap.ofList [(5,"a"); (3,"b")])
    |> should equal ("Everything: ba", IntMap.ofList [(3, "bX"); (5, "aX")])

[<Test>]
let ``test mapAccumWithKey``() =
    let f a k b = (a + " " + (string k) + "-" + b, b + "X")
    IntMap.mapAccumWithKey f "Everything:" (IntMap.ofList [(5,"a"); (3,"b")])
    |> should equal ("Everything: 3-b 5-a", IntMap.ofList [(3, "bX"); (5, "aX")])

[<Test>]
let ``test mapKeys``() =
    IntMap.mapKeys (flip (+) 1) (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(4, "b"); (6, "a")])
    IntMap.mapKeys (fun _ -> 1) (IntMap.ofList [(1,"b"); (2,"a"); (3,"d"); (4,"c")]) |> should equal (IntMap.singleton 1 "c")
    IntMap.mapKeys (fun _ -> 3) (IntMap.ofList [(1,"b"); (2,"a"); (3,"d"); (4,"c")]) |> should equal (IntMap.singleton 3 "c")

[<Test>]
let ``test mapKeysWith``() =
    IntMap.mapKeysWith (+) (fun _ -> 1) (IntMap.ofList [(1,"b"); (2,"a"); (3,"d"); (4,"c")]) |> should equal (IntMap.singleton 1 "cdab")
    IntMap.mapKeysWith (+) (fun _ -> 3) (IntMap.ofList [(1,"b"); (2,"a"); (3,"d"); (4,"c")]) |> should equal (IntMap.singleton 3 "cdab")

// Conversion

[<Test>]
let ``test values``() =
    IntMap.values (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal ["b";"a"]
    IntMap.values IntMap.empty |> should equal []

[<Test>]
let ``test keys``() =
    IntMap.keys (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal [3;5]
    IntMap.keys IntMap.empty |> should equal IntMap.empty

// Lists

[<Test>]
let ``test toList``() =
    IntMap.toList (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal [(3,"b"); (5,"a")]
    IntMap.toList IntMap.empty |> should equal []

[<Test>]
let ``test ofList``() =
    IntMap.ofList [] |> should equal IntMap.empty
    IntMap.ofList [(5,"a"); (3,"b"); (5, "c")] |> should equal (IntMap.ofList [(5,"c"); (3,"b")])
    IntMap.ofList [(5,"c"); (3,"b"); (5, "a")] |> should equal (IntMap.ofList [(5,"a"); (3,"b")])

[<Test>]
let ``test ofListWith``() =
    IntMap.ofListWith (+) [(5,"a"); (5,"b"); (3,"b"); (3,"a"); (5,"a")] |> should equal (IntMap.ofList [(3, "ab"); (5, "aba")])
    IntMap.ofListWith (+) [] |> should equal IntMap.empty

[<Test>]
let ``test ofListWithKey``() =
    let f k a1 a2 = (string k) + a1 + a2
    IntMap.ofListWithKey f [(5,"a"); (5,"b"); (3,"b"); (3,"a"); (5,"a")] |> should equal (IntMap.ofList [(3, "3ab"); (5, "5a5ba")])
    IntMap.ofListWithKey f [] |> should equal IntMap.empty

// Filter

[<Test>]
let ``test filter``() =
    IntMap.filter (flip (>) "a") (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 3 "b")
    IntMap.filter (flip (>) "x") (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal IntMap.empty
    IntMap.filter (flip (<) "a") (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal IntMap.empty

[<Test>]
let ``test filteWithKey``() =
    IntMap.filterWithKey (fun k _ -> k > 4) (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 5 "a")

[<Test>]
let ``test partition``() =
    let left, right = IntMap.partition (flip (>) "a") (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.singleton 3 "b")
    right |> should equal (IntMap.singleton 5 "a")
    let left, right = IntMap.partition (flip (<) "x") (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.ofList [(3, "b"); (5, "a")])
    right |> should equal IntMap.empty
    let left, right = IntMap.partition (flip (>) "x") (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal IntMap.empty
    right |> should equal (IntMap.ofList [(3, "b"); (5, "a")])

[<Test>]
let ``test partitionWithKey``() =
    let left, right = IntMap.partitionWithKey (fun k _ -> k > 3) (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.singleton 5 "a")
    right |> should equal (IntMap.singleton 3 "b")
    let left, right = IntMap.partitionWithKey (fun k _ -> k < 7) (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.ofList [(3, "b"); (5, "a")])
    right |> should equal IntMap.empty
    let left, right = IntMap.partitionWithKey (fun k _ -> k > 7) (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal IntMap.empty
    right |> should equal (IntMap.ofList [(3, "b"); (5, "a")])

[<Test>]
let ``test mapOption``() =
    let f x = if x = "a" then Some "new a" else None
    IntMap.mapOption f (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 5 "new a")

[<Test>]
let ``test mapOptionWithKey``() =
    let f k _ = if k < 5 then Some ("key : " + (string k)) else None
    IntMap.mapOptionWithKey f (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 3 "key : 3")

[<Test>]
let ``test mapChoice``() =
    let f a = if a < "c" then Choice1Of2 a else Choice2Of2 a
    IntMap.mapChoice f (IntMap.ofList [(5,"a"); (3,"b"); (1,"x"); (7,"z")])
    |> should equal (IntMap.ofList [(3,"b"); (5,"a")], IntMap.ofList [(1,"x"); (7,"z")])
    IntMap.mapChoice (fun a -> Choice2Of2 a) (IntMap.ofList [(5,"a"); (3,"b"); (1,"x"); (7,"z")])
    |> should equal (IntMap.empty, IntMap.ofList [(5,"a"); (3,"b"); (1,"x"); (7,"z")])

[<Test>]
let ``test mapChoiceWithKey``() =
    let f k a = if k < 5 then Choice1Of2 (k * 2) else Choice2Of2 (a + a)
    IntMap.mapChoiceWithKey f (IntMap.ofList [(5,"a"); (3,"b"); (1,"x"); (7,"z")])
    |> should equal (IntMap.ofList [(1,2); (3,6)], IntMap.ofList [(5,"aa"); (7,"zz")])
    IntMap.mapChoiceWithKey (fun _ a -> Choice2Of2 a) (IntMap.ofList [(5,"a"); (3,"b"); (1,"x"); (7,"z")])
    |> should equal (IntMap.empty, IntMap.ofList [(1,"x"); (3,"b"); (5,"a"); (7,"z")])

[<Test>]
let ``test split``() =
    let left, right = IntMap.split 2 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal IntMap.empty
    right |> should equal (IntMap.ofList [(3,"b"); (5,"a")])
    let left, right = IntMap.split 3 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal IntMap.empty
    right |> should equal (IntMap.singleton 5 "a")
    let left, right = IntMap.split 4 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.singleton 3 "b")
    right |> should equal (IntMap.singleton 5 "a")
    let left, right = IntMap.split 5 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.singleton 3 "b")
    right |> should equal IntMap.empty
    let left, right = IntMap.split 6 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.ofList [(3,"b"); (5,"a")])
    right |> should equal IntMap.empty

[<Test>]
let ``test splitTryFind``() =
    let left, center, right = IntMap.splitTryFind 2 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal IntMap.empty
    center |> should equal None
    right |> should equal (IntMap.ofList [(3,"b"); (5,"a")])
    let left, center, right = IntMap.splitTryFind 3 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal IntMap.empty
    center |> should equal (Some "b")
    right |> should equal (IntMap.singleton 5 "a")
    let left, center, right = IntMap.splitTryFind 4 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.singleton 3 "b")
    center |> should equal None
    right |> should equal (IntMap.singleton 5 "a")
    let left, center, right = IntMap.splitTryFind 5 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.singleton 3 "b")
    center |> should equal (Some "a")
    right |> should equal IntMap.empty
    let left, center, right = IntMap.splitTryFind 6 (IntMap.ofList [(5,"a"); (3,"b")])
    left |> should equal (IntMap.ofList [(3,"b"); (5,"a")])
    center |> should equal None
    right |> should equal IntMap.empty

// Submap

[<Test>]
let ``test isSubmapOfBy``() =
    IntMap.isSubmapOfBy (=) (IntMap.ofList [(int 'a',1)]) (IntMap.ofList [(int 'a',1);(int 'b',2)]) |> should be True
    IntMap.isSubmapOfBy (<=) (IntMap.ofList [(int 'a',1)]) (IntMap.ofList [(int 'a',1);(int 'b',2)]) |> should be True
    IntMap.isSubmapOfBy (=) (IntMap.ofList [(int 'a',1);(int 'b',2)]) (IntMap.ofList [(int 'a',1);(int 'b',2)]) |> should be True
    IntMap.isSubmapOfBy (=) (IntMap.ofList [(int 'a',2)]) (IntMap.ofList [(int 'a',1);(int 'b',2)]) |> should be False
    IntMap.isSubmapOfBy (<)  (IntMap.ofList [(int 'a',1)]) (IntMap.ofList [(int 'a',1);(int 'b',2)]) |> should be False
    IntMap.isSubmapOfBy (=) (IntMap.ofList [(int 'a',1);(int 'b',2)]) (IntMap.ofList [(int 'a',1)]) |> should be False

[<Test>]
let ``test isSubmapOf``() =
    IntMap.isSubmapOf (IntMap.ofList [(int 'a',1)]) (IntMap.ofList [(int 'a',1);(int 'b',2)]) |> should be True
    IntMap.isSubmapOf (IntMap.ofList [(int 'a',1);(int 'b',2)]) (IntMap.ofList [(int 'a',1);(int 'b',2)]) |> should be True
    IntMap.isSubmapOf (IntMap.ofList [(int 'a',2)]) (IntMap.ofList [(int 'a',1);(int 'b',2)]) |> should be False
    IntMap.isSubmapOf (IntMap.ofList [(int 'a',1);(int 'b',2)]) (IntMap.ofList [(int 'a',1)]) |> should be False

[<Test>]
let ``test isProperSubmapOfBy``() =
    IntMap.isProperSubmapOfBy (=) (IntMap.ofList [(1,1)]) (IntMap.ofList [(1,1);(2,2)]) |> should be True
    IntMap.isProperSubmapOfBy (<=) (IntMap.ofList [(1,1)]) (IntMap.ofList [(1,1);(2,2)]) |> should be True
    IntMap.isProperSubmapOfBy (=) (IntMap.ofList [(1,1);(2,2)]) (IntMap.ofList [(1,1);(2,2)]) |> should be False
    IntMap.isProperSubmapOfBy (=) (IntMap.ofList [(1,1);(2,2)]) (IntMap.ofList [(1,1)]) |> should be False
    IntMap.isProperSubmapOfBy (<)  (IntMap.ofList [(1,1)]) (IntMap.ofList [(1,1);(2,2)]) |> should be False

[<Test>]
let ``test isProperSubmapOf``() =
    IntMap.isProperSubmapOf (IntMap.ofList [(1,1)]) (IntMap.ofList [(1,1);(2,2)]) |> should be True
    IntMap.isProperSubmapOf (IntMap.ofList [(1,1);(2,2)]) (IntMap.ofList [(1,1);(2,2)]) |> should be False
    IntMap.isProperSubmapOf (IntMap.ofList [(1,1);(2,2)]) (IntMap.ofList [(1,1)]) |> should be False

// Min/Max

[<Test>]
let ``test findMin``() =
    IntMap.findMin (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (3,"b")

[<Test>]
let ``test findMax``() =
    IntMap.findMax (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (5,"a")

[<Test>]
let ``test deleteMin``() =
    IntMap.deleteMin (IntMap.ofList [(5,"a"); (3,"b"); (7,"c")]) |> should equal (IntMap.ofList [(5,"a"); (7,"c")])
    IntMap.deleteMin IntMap.empty |> should equal IntMap.empty

[<Test>]
let ``test deleteMax``() =
    IntMap.deleteMax (IntMap.ofList [(5,"a"); (3,"b"); (7,"c")]) |> should equal (IntMap.ofList [(3,"b"); (5,"a")])
    IntMap.deleteMax IntMap.empty |> should equal IntMap.empty

[<Test>]
let ``test deleteFindMin``() =
    IntMap.deleteFindMin (IntMap.ofList [(5,"a"); (3,"b"); (10,"c")]) |> should equal ((3,"b"), IntMap.ofList [(5,"a"); (10,"c")])

[<Test>]
let ``test deleteFindMax``() =
    IntMap.deleteFindMax (IntMap.ofList [(5,"a"); (3,"b"); (10,"c")]) |> should equal ((10,"c"), IntMap.ofList [(3,"b"); (5,"a")])

[<Test>]
let ``test updateMin``() =
    IntMap.updateMin (fun a -> Some ("X" + a)) (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "Xb"); (5, "a")])
    IntMap.updateMin (fun _ -> None) (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 5 "a")

[<Test>]
let ``test updateMax``() =
    IntMap.updateMax (fun a -> Some ("X" + a)) (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.ofList [(3, "b"); (5, "Xa")])
    IntMap.updateMax (fun _ -> None) (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 3 "b")

[<Test>]
let ``test updateMinWithKey``() =
    IntMap.updateMinWithKey (fun k a -> Some ((string k) + ":" + a)) (IntMap.ofList [(5,"a"); (3,"b")])
    |> should equal (IntMap.ofList [(3,"3:b"); (5,"a")])
    IntMap.updateMinWithKey (fun _ _ -> None) (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (IntMap.singleton 5 "a")

[<Test>]
let ``test updateMaxWithKey``() =
    IntMap.updateMaxWithKey (fun k a -> Some ((string k) + ":" + a)) (IntMap.ofList [(5,"a"); (3,"b")])
    |> should equal (IntMap.ofList [(3,"b"); (5,"5:a")])
    IntMap.updateMaxWithKey (fun _ _ -> None) (IntMap.ofList [(5,"a"); (3,"b")])
    |> should equal (IntMap.singleton 3 "b")

[<Test>]
let ``test minView``() =
    IntMap.minView (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (Some ("b", IntMap.singleton 5 "a"))
    IntMap.minView IntMap.empty |> should equal None

[<Test>]
let ``test maxView``() =
    IntMap.maxView (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (Some ("a", IntMap.singleton 3 "b"))
    IntMap.maxView IntMap.empty |> should equal None

[<Test>]
let ``test minViewWithKey``() =
    IntMap.minViewWithKey (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (Some ((3,"b"), IntMap.singleton 5 "a"))
    IntMap.minViewWithKey IntMap.empty |> should equal None

[<Test>]
let ``test maxViewWithKey``() =
    IntMap.maxViewWithKey (IntMap.ofList [(5,"a"); (3,"b")]) |> should equal (Some ((5,"a"), IntMap.singleton 3 "b"))
    IntMap.maxViewWithKey IntMap.empty |> should equal None

open FsCheck
open FsCheck.NUnit
open FSharpx.Tests.Properties
open System.Linq

let fsCheck t = fsCheck "" t

type IntMapGen =
    static member IntMap() =
        let rec intMapGen() = 
            gen {
                let! ks = Arb.generate
                let! xs = Arb.generate
                return IntMap.ofSeq (Seq.zip (Seq.ofList xs) (Seq.ofList ks))
            }
        Arb.fromGen (intMapGen())

let registerGen = lazy (Arb.register<IntMapGen>() |> ignore)

[<Test>]
let ``prop singleton``() =
    fsCheck (fun k x -> IntMap.insert k x IntMap.empty = IntMap.singleton k x)

[<Test>]
let ``prop insert and tryFind``() =
    registerGen.Force()
    fsCheck (fun k t -> IntMap.tryFind k (IntMap.insert k () t) <> None)

[<Test>]
let ``prop insert and delete``() =
    registerGen.Force()
    fsCheck <| fun k t ->
        IntMap.tryFind k t = None ==> (IntMap.delete k (IntMap.insert k () t) = t)

[<Test>]
let ``prop delete non member``() =
    registerGen.Force()
    fsCheck <| fun k t ->
        IntMap.tryFind k t = None ==> (IntMap.delete k t = t)

[<Test>]
let ``prop append``() =
    fsCheck <| fun xs ys ->
        List.sort (IntMap.keys (IntMap.append (IntMap.ofList xs) (IntMap.ofList ys)))
            = List.sort (List.ofSeq (Seq.distinct (List.append (List.map fst xs) (List.map fst ys))))

[<Test>]
let ``prop append and singleton``() =
    registerGen.Force()
    fsCheck (fun t k x -> IntMap.append (IntMap.singleton k x) t = IntMap.insert k x t)

[<Test>]
let ``prop append and sum``() =
    fsCheck <| fun xs ys ->
        List.sum (IntMap.values (IntMap.appendWith (+) (IntMap.ofListWith (+) xs) (IntMap.ofListWith (+) ys)))
            = List.sum (List.map snd xs) + List.sum (List.map snd ys)

let except (xs: _ seq) ys = xs.Except(ys)
let intersect (xs: _ seq) ys = xs.Intersect(ys)

[<Test>]
let ``prop difference``() =
    fsCheck <| fun xs ys ->
        List.sort (IntMap.keys (IntMap.difference (IntMap.ofListWith (+) xs) (IntMap.ofListWith (+) ys)))
            = List.sort (List.ofSeq (except (Seq.distinct (List.map fst xs)) (Seq.distinct (List.map fst ys))))

[<Test>]
let ``prop intersection``() =
    fsCheck <| fun xs ys ->
        List.sort (IntMap.keys (IntMap.intersection (IntMap.ofListWith (+) xs) (IntMap.ofListWith (+) ys)))
            = List.sort (List.ofSeq (Seq.distinct (intersect (List.map fst xs) (List.map fst ys))))

[<Test>]
let ``prop intersectionWith``() =
    fsCheck <| fun (xs: (int * int) list) (ys: (int * int) list) ->
        let xs' = Seq.distinctBy fst xs |> Seq.toList
        let ys' = Seq.distinctBy fst ys |> Seq.toList
        let f l r = l + 2 * r
        IntMap.toList (IntMap.intersectionWith f (IntMap.ofList xs') (IntMap.ofList ys'))
            = [ for (kx, vx) in List.sort xs' do for (ky, vy) in ys' do if kx = ky then yield (kx, f vx vy) ]

[<Test>]
let ``prop intersectionWithKey``() =
    fsCheck <| fun (xs: (int * int) list) (ys: (int * int) list) ->
        let xs' = Seq.distinctBy fst xs |> Seq.toList
        let ys' = Seq.distinctBy fst ys |> Seq.toList
        let f k l r = k + 2 * l + 3 * r
        IntMap.toList (IntMap.intersectionWithKey f (IntMap.ofList xs') (IntMap.ofList ys'))
            = [ for (kx, vx) in List.sort xs' do for (ky, vy) in ys' do if kx = ky then yield (kx, f kx vx vy) ]

let mapOption (f: 'a -> 'b option) l = List.foldBack (fun x xs -> match f x with Some v -> v::xs | None -> xs) l []

[<Test>]
let ``prop mergeWithKey``() =
    fsCheck <| fun (xs: (int * int) list) (ys: (int * int) list) ->
        let xs' = Seq.distinctBy fst xs |> Seq.toList
        let ys' = Seq.distinctBy fst ys |> Seq.toList
        let xm = IntMap.ofList xs'
        let ym = IntMap.ofList ys'

        let emulateMergeWithKey f keep_x keep_y =
            let combine k =
                match (List.tryFind (fst >> (=) k) xs', List.tryFind (fst >> (=) k) ys') with
                | (None, Some(_, y)) -> if keep_y then Some (k, y) else None
                | (Some(_, x), None) -> if keep_x then Some (k, x) else None
                | (Some(_, x), Some(_, y)) ->  f k x y |> Option.map (fun v -> (k, v))
                | _ -> failwith "emulateMergeWithKey: combine"
            mapOption combine (List.sort (List.ofSeq (Seq.distinct (List.append (List.map fst xs') (List.map fst ys')))))

        let testMergeWithKey f keep_x keep_y =
            let keep b m = match b with | false -> IntMap.empty | true -> m
            IntMap.toList (IntMap.mergeWithKey f (keep keep_x) (keep keep_y) xm ym) = emulateMergeWithKey f keep_x keep_y
                    
        List.forall id [ for f in
            [ (fun _ x1 _ -> Some x1);
                (fun _ _ x2 -> Some x2);
                (fun _ _ _ -> None);
                (fun k x1 x2 -> if k % 2 = 0 then None else Some (2 * x1 + 3 * x2))
            ] do
                for keep_x in [ true; false ] do
                    for keep_y in [ true; false ] do yield testMergeWithKey f keep_x keep_y
        ]

[<Test>]
let ``prop list``() =
    fsCheck <| fun (xs: int list) ->
        List.sort (List.ofSeq (Seq.distinct xs)) = [ for (x,()) in IntMap.toList (IntMap.ofList [ for x in xs do yield (x,()) ]) do yield x ]

[<Test>]
let ``prop alter``() =
    fsCheck <| fun t k ->
        let f = function | Some () -> None | None -> Some ()
        let t' = IntMap.alter f k t
        match IntMap.tryFind k t with
        | Some _ -> IntMap.size t - 1 = IntMap.size t' && IntMap.tryFind k t' = None
        | None -> IntMap.size t + 1 = IntMap.size t' && IntMap.tryFind k t' <> None

[<Test>]
let ``prop isEmpty``() =
    registerGen.Force()
    fsCheck (fun m -> IntMap.isEmpty m = (IntMap.size m = 0))

[<Test>]
let ``prop exists``() =
    fsCheck <| fun xs n ->
        let m = IntMap.ofList (List.zip xs xs)
        List.forall (fun k -> IntMap.exists k m = List.exists ((=) k) xs) (n::xs)

[<Test>]
let ``prop notExists``() =
    fsCheck <| fun xs n ->
        let m = IntMap.ofList (List.zip xs xs)
        List.forall (fun k -> IntMap.notExists k m = not (List.exists ((=) k) xs)) (n::xs)

[<Test>]
let ``implements IEnumerable``() =
    fsCheck <| fun xs ->
        let xs = List.zip xs xs
        let map = IntMap.ofList xs
        let a = map :> _ seq |> Seq.toList
        set xs = set a && a.Length = List.length (List.ofSeq (Seq.distinct xs))

[<Test>]
let ``functor laws``() =
    registerGen.Force()
    let fmap = IntMap.map
    let n = sprintf "IntMap : functor %s"
    NUnit.fsCheck (n "preserves identity") <| 
        fun value -> fmap id value = value
    NUnit.fsCheck (n "preserves composition") <|
        fun f g value -> fmap (f << g) value = (fmap f << fmap g) value

[<Test>]
let ``monoid law``() =
    registerGen.Force()
    checkMonoid "IntMap" (IntMap.monoid)
