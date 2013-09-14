module FSharpx.Collections.Tests.SeqTests

open System
open FSharpx.Collections
open FsCheck
open FsCheck.NUnit
open NUnit.Framework
open FsUnit

[<Test>]
let index() =
  let a = {'a'..'z'}
  Seq.index a |> Seq.take 5 |> Seq.toList
  |> should equal [0,'a'; 1,'b'; 2,'c'; 3,'d'; 4,'e']

[<Test>]
let tryFindWithIndex_None() =
  let a = {1..10}
  Seq.tryFindWithIndex ((<)10) a
  |> should equal None

[<Test>]
let tryFindWithIndex_Some() =
  let a = {'a'..'z'}
  Seq.tryFindWithIndex ((=)'e') a
  |> should equal (Some (4,'e'))

[<Test>]
let lift2() =
    Seq.lift2 (+) [0;1] [0;2] |> Seq.toList
    |> should equal [0;2;1;3]
    
let data = [1.;2.;3.;4.;5.;6.;7.;8.;9.;10.]

[<Test>]
let ``I should be able to break the iteration of a seq based on a predicate``() =
    let result = ref []
    Seq.iterBreak (fun a -> result := a :: !result;  a <= 5.) data
    !result |> List.rev |> should equal [1.;2.;3.;4.;5.;6.]

[<Test>]
let ``If I tryAverage an empty seq I should get none``() =
    Seq.empty<float> 
    |> Seq.tryAverage
    |> should equal None

[<Test>]
let ``If I tryAverage a none empty seq I should get the average``() =
    data |> Seq.tryAverage
    |> should equal (Some (5.5))

[<Test>]
let ``I should be a to split a seq at an index``() =
    let (a,b) = Seq.splitAt 5 data
    a |> should equal [1.;2.;3.;4.;5.]
    b |> should equal [6.;7.;8.;9.;10.]

[<Test>]
let ``I should be able to turn a stream reader into a sequence``() =
    use data = new IO.MemoryStream(Text.Encoding.UTF8.GetBytes("1\r\n2\r\n3\r\n"))
    use reader = new IO.StreamReader(data)
    Seq.ofStreamReader reader
    |> should equal ["1";"2";"3"]

[<Test>]
let ``I should be able to turn a stream into a sequence of bytes``() =
    let bytes = Text.Encoding.UTF8.GetBytes("1\r\n2\r\n3\r\n")
    use stream = new IO.MemoryStream(bytes)
    Seq.ofStreamByByte stream
    |> should equal bytes

[<Test>]
let ``I should be able to turn a stream into a chunked sequence of bytes``() =
    let bytes = Text.Encoding.UTF8.GetBytes("1\r\n2\r\n3\r\n")
    use stream = new IO.MemoryStream(bytes)
    Seq.ofStreamByChunk 3 stream
    |> should equal ([
                        Text.Encoding.UTF8.GetBytes("1\r\n")
                        Text.Encoding.UTF8.GetBytes("2\r\n")
                        Text.Encoding.UTF8.GetBytes("3\r\n")
                        ] |> List.toSeq)

[<Test>]
let ``I should be able to create a inifinite seq of values``() =
    let data = [1;2]
    Seq.asCircular [1;2] |> Seq.take 4
    |> should equal [1;2;1;2]

[<Test>]
let ``I should be able to create a inifinite seq of values and have none in between each iteration``() =
    let data = [1;2]
    Seq.asCircularWithBreak [1;2] |> Seq.take 5
    |> should equal [Some 1;Some 2; None; Some 1; Some 2]

[<Test>]
let ``I should be able to create a inifinite seq of values and call function when seq exhusted``() =
    let called = ref false
    let data = [1;2]
    Seq.asCircularOnLoop (fun () -> called := true) [1;2] 
    |> Seq.take 4
    |> should equal [1;2;1;2]

[<Test>]
let ``I should get none if try to get a index outside the seq``() =
    Seq.tryNth 20 data
    |> should equal None

[<Test>]
let ``I should get some if try to get a index inside the seq``() =
    Seq.tryNth 2 data
    |> should equal (Some(3.))

[<Test>]
let ``I should get none when trySkip past the end of the seq``() =
    Seq.skipNoFail 20 data
    |> should equal Seq.empty

[<Test>]
let ``I should get Some when trySkip``() =
    Seq.skipNoFail 5 data
    |> should equal [6.;7.;8.;9.;10.]

[<Test>] 
let ``I should be able to repeat a single value infinitely``() =
    Seq.repeat 1 |> Seq.take 5
    |> should equal [1;1;1;1;1]

[<Test>]
let ``I should be able to get the tail of a sequence``() =
    Seq.tail [1;2;3;4]
    |> should equal [2;3;4]

[<Test>]
[<ExpectedException(typeof<ArgumentException>)>]
let ``I should not be able to get the tail of a empty sequence``() =
    Seq.tail []
    |> should equal []

[<Test>]
let ``I should be able to get the tail of a empty sequence without a fail``() =
    Seq.tailNoFail []
    |> should equal []

[<Test>]
let ``I should be able to contract a seq taking every nth value``() =
    Seq.contract 5 data
    |> should equal [5;10]
    
[<Test>]
let ``I should be able to contract a seq sequence by a given ratio``() = 
    let actual = Seq.contract 2 (Seq.init 72 (fun i -> 0)) |> Seq.toList
    let expected = Seq.init 36 (fun i -> 0) |> Seq.toList
    actual |> should equal expected

[<Test>]
let ``I should be able to contract an empty sequence``() =
    let actual = Seq.contract 5 (Seq.empty)
    actual |> should equal [] 

[<Test>]
let ``I should be able to contract a infinite sequence``() =
    let actual = Seq.contract 5 (Seq.initInfinite (fun i -> i + 1))
    actual |> Seq.take 5 |> should equal [5;10;15;20;25] 


[<Test>]
let ``Should be able to combine two sequences``() =
    let a,b = [1;2;3;4;5], [6;7;8;9;10]
    Seq.combine (+) a b
    |> should equal [7;9;11;13;15]

[<Test>]
let ``Should be able to combine two empty sequences``() =
    let a,b = [], []
    Seq.combine (+) a b
    |> should equal []

[<Test>]
let ``Should be able to combine two sequences when one is infinite``() =
    let a,b = [1;2;3;4;5], (Seq.initInfinite (fun i -> i + 6))
    Seq.combine (+) a b |> Seq.take 5
    |> should equal [7;9;11;13;15]

[<Test>]
let ``Should be able to combine two sequences when both are infinite``() =
    let a,b = (Seq.initInfinite (fun i -> i + 1)), (Seq.initInfinite (fun i -> i + 6))
    Seq.combine (+) a b |> Seq.take 5
    |> should equal [7;9;11;13;15]

[<Test>]
let ``I should be able to expand a seq``() =
    let a = [1;2;3;4;5]
    Seq.grow 2 a
    |> should equal [1;1;2;2;3;3;4;4;5;5]

[<Test>]
let ``I should be able to page a seq``() =
    Seq.page 0 2 data |> should equal [1.;2.]
    Seq.page 1 2 data |> should equal [3.;4.]
    Seq.page 2 2 data |> should equal [5.;6.]


[<Test>]
let ``I should intersperse a seq``() = 
    let a = "foobar".ToCharArray()

    let expected = ['f';',';'o';',';'o';',';'b';',';'a';',';'r']

    (a |> Seq.intersperse ',') |> should equal expected

[<Test>]
let ``I shouldn't interperse an empty list``() = 
    let a = []

    (a |> Seq.intersperse ',') |> should equal a

[<Test>]
let ``I should interperse always 2n-1 elements``() = 
    let intersperse elem (list:'a list) = 
        let interpersed = Seq.intersperse elem list

        if Seq.length list = 0 then 
            Seq.length interpersed = 0
        else
            Seq.length interpersed = (Seq.length list) * 2 - 1
            
    fsCheck "interperse a list" intersperse 




