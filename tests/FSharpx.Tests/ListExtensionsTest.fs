module FSharpx.Tests.ListExtensionsTest

open FSharpx
open NUnit.Framework
open FsUnit

[<Test>]
let ``test List_split correctly breaks the list on the specified predicate``() =
  let str = List.ofSeq "Howdy! Want to play?"
  let expected = (List.ofSeq "Howdy!", List.ofSeq " Want to play?")
  List.split (fun c -> c = ' ') str |> should equal expected

[<Test>]
let ``test List_splitAt correctly breaks the list on the specified index``() =
  let str = List.ofSeq "Howdy! Want to play?"
  let expected = (List.ofSeq "Howdy!", List.ofSeq " Want to play?")
  List.splitAt 6 str |> should equal expected

  
[<Test>]
let ``Can splitAt 3``() =
  let list = [1..5]
  let expected = [1;2;3],[4;5]
  List.splitAt 3 list |> should equal expected

[<Test>]
let ``Can split at 3``() =
  let list = [1..5]
  let expected = [1;2],[3;4;5]
  List.split ((=) 3) list |> should equal expected

[<Test>]
let ``Can split at 0``() =
  let l1,l2 = List.split ((=) 0) [1..5]
  l1 |> should equal [1..5]
  l2 |> should equal []

[<Test>]
let ``test List_span correctly breaks the list on the specified predicate``() =
  let str = List.ofSeq "Howdy! Want to play?"
  let expected = (List.ofSeq "Howdy!", List.ofSeq " Want to play?")
  List.span (fun c -> c <> ' ') str |> should equal expected

[<Test>]
let lift2() =
    List.lift2 (+) [0;1] [0;2]
    |> should equal [0;2;1;3]

[<Test>]
let mapAccum() =
  let list = [-5..-1]
  let expected = (15, [5;4;3;2;1])
  List.mapAccum (fun a b -> let c = abs b in (a+c,c)) 0 list |> should equal expected
