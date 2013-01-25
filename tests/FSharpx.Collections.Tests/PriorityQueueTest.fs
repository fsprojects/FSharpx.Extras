module FSharpx.Collections.Tests.PriorityQueueTest

open FSharpx
open FSharpx.Collections.PriorityQueue
open NUnit.Framework
open FsUnit

[<Test>]
let ``empty queue should be empty``() =
    let pq = empty false

    isEmpty pq |> should equal true
    tryPeek pq |> should equal None
    tryPop pq |> should equal None

[<Test>]
let ``After adding an element to the PQ it shouldn't be empty``() =
    let pq = empty false |> insert 1

    isEmpty pq |> should equal false
    

[<Test>]
let ``After adding an element to the PQ the element should be the smallest``() =
    let pq = empty false |> insert 1

    tryPeek pq |> should equal (Some 1)
    peek pq |> should equal 1

[<Test>]
let ``After adding an element to the PQ and popping it the PQ should be empty``() =
    let pq = empty false |> insert 1

    let element,newPQ = pop pq
    element |> should equal 1
    isEmpty newPQ |> should equal true

    let element,newPQ = (tryPop pq).Value
    element |> should equal 1
    isEmpty newPQ |> should equal true

[<Test>]
let ``Adding multiple elements to the PQ should allow to pop the smallest``() =
    let pq = empty false |> insert 1 |> insert 3 |> insert 0 |> insert 4 |> insert -3

    let element,newPQ = pop pq
    element |> should equal -3

    let element,newPQ = pop newPQ
    element |> should equal 0

    let element,newPQ = pop newPQ
    element |> should equal 1

    let element,newPQ = pop newPQ
    element |> should equal 3

    let element,newPQ = pop newPQ
    element |> should equal 4

    isEmpty newPQ |> should equal true

[<Test>]
let ``Adding multiple elements to a MaxPriorityQueue should allow to pop the smallest``() =
    let pq = empty true |> insert 1 |> insert 3 |> insert 0 |> insert 4 |> insert -3

    let element,newPQ = pop pq
    element |> should equal 4

    let element,newPQ = pop newPQ
    element |> should equal 3

    let element,newPQ = pop newPQ
    element |> should equal 1

    let element,newPQ = pop newPQ
    element |> should equal 0

    let element,newPQ = pop newPQ
    element |> should equal -3

    isEmpty newPQ |> should equal true

[<Test>]
let ``Can use a PQ as a seq``() =
    let pq = empty false |> insert 15 |> insert 3 |> insert 0 |> insert 4 |> insert -3

    pq |> Seq.toList |> should equal [-3;0;3;4;15]