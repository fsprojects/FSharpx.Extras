module FSharpx.DataStructures.Tests.RingBufferTest

open System
open FSharpx.DataStructures
open FSharpx.DataStructures.RingBuffer
open NUnit.Framework
open FsUnit

[<Test>]
let ``empty RingBuffer should not contain any elements``() =
    let rb = create []
    rb.Length |> should equal 0
    rb.Values |> should equal []

[<Test>]
let ``Creating a RingBuffer with a seq should init the RingBuffer``() =
    let rb = create [1..100]
    rb.Length |> should equal 100
    rb.Values |> should equal [1..100]

[<Test>]
let ``Creating a ring buffer with a given size``() = 
    let actual = new RingBuffer<int>(10)
    let expected = Array.init 10 (fun _ -> 0)
    actual.Values |> should equal expected

[<Test>]
let ``Creating a ring buffer with a given set of values``() = 
    let actual = new RingBuffer<int>([|1;2;3;4;5;6;7;8;9;10|])
    let expected = Array.init 10 (fun i -> i + 1)
    actual.Values |> should equal expected

[<Test>]
let ``Inserting values at 0 offset should prepend them``() = 
    let buffer = new RingBuffer<int>(10)
    buffer.Insert(0, [|1;2;3;4;5|])
    buffer.Values |> should equal [|1;2;3;4;5;0;0;0;0;0|]

[<Test>]
let ``When inserting more values than the size of the buffer it should take only the first``() =
    let buffer = new RingBuffer<int>(10)
    buffer.Insert(0, [|1;2;3;4;5;6;7;8;9;10;11|])
    buffer.Values |> should equal [|1;2;3;4;5;6;7;8;9;10|]

[<Test>]
let ``Insert should not fail if an empty sequence is inserted``() = 
    let actual = new RingBuffer<int>(10)
    actual.Insert(0, [||])
    let expected = Array.init 10 (fun _ -> 0)
    actual.Values |> should equal expected

[<Test>]
let ``Advancing past the end of the buffer should cause it to wrap``() =
    let buffer = new RingBuffer<int>([|1;2;3;4;5;6;7;8;9;10|])
    buffer.Advance(11)
    buffer.Values |> should equal [|0;0;0;0;0;0;0;0;0;0|] 
    buffer.Position |> should equal 1      

[<Test>]
let ``Cloning a ring buffer should give a new copy``() = 
    let buffer = new RingBuffer<int>([|1;2;3;4;5;6;7;8;9;10|])
    let clone = buffer.Clone()
    buffer.Values |> should equal clone.Values
    clone.Advance(2)
    Assert.AreNotEqual(buffer.Values,clone.Values)
    buffer.Position |> should equal 0
    clone.Position |> should equal 2