module FSharpx.Collections.Experimental.Tests.PersistentVectorTest

open System
open FSharpx.Collections.Experimental
open FSharpx.Collections.Experimental.Vector
open NUnit.Framework
open FsUnit

[<Test>]
let ``empty vector should be empty``() =
    let x = empty<int>
    x |> count |> should equal 0

[<Test>]
let ``multiple cons to an empty vector should increase the count``() =
    empty |> conj 1 |> conj 4 |> conj 25 |> count |> should equal 3

[<Test>]
let ``cons to an empty vector should create a singleton vector``() =
    empty |> conj 1 |> nth 0 |> should equal 1

[<Test>]
let ``multiple cons to an empty vector should create a vector``() =
    empty |> conj 1 |> conj 4 |> conj 25 |> nth 1 |> should equal 4

[<Test>]
let ``multiple assoc to the end should work like cons and create a vector``() =
    let v = empty |> assocN 0 1 |> assocN 1 4 |> assocN 2 25 
    v |> nth 0 |> should equal 1
    v |> nth 1 |> should equal 4
    v |> nth 2 |> should equal 25

[<Test>]
let ``300 cons to an empty vector should create a vector``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    !vector |> nth 100 |> should equal 101
    !vector |> nth 200 |> should equal 201

[<Test>]
let ``assoc an element to a nonempty vector should not change the original vector``() =
    let v = empty |> conj "1" |> conj "4" |> conj "25" 

    v |> assocN 2 "5" |> nth 2 |> should equal "5"
    v |> nth 2 |> should equal "25"

[<Test>]
let ``vector should should be convertable to a seq``() =
    empty |> conj 1 |> conj 4 |> conj 25  |> Seq.toList |> should equal [1;4;25]

[<Test>]
let ``vector with 300 elements should be convertable to a seq``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    let a = !vector |> Seq.toArray 
    for i in 1..300 do i |> should equal a.[i-1]

[<Test>]
let ``vector can be created from a seq``() =
    let xs = [7;88;1;4;25;30] 
    ofSeq xs |> Seq.toList |> should equal xs

[<Test>]
let ``vector with 300 elements should allow assocN``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    for i in 1..300 do
        vector := assocN (i-1) (i*2) (!vector)

    let a = !vector |> Seq.toArray 
    for i in 1..300 do i * 2 |> should equal a.[i-1]

[<Test>]
let ``can peek elements from a vector``() =
    let vector = empty |> conj 1 |> conj 4 |> conj 25 
    vector |> peek |> should equal 25
    
[<Test>]
let ``can pop elements from a vector``() =
    let vector = empty |> conj 1 |> conj 4 |> conj 25 
    vector |> peek |> should equal 25
    vector |> pop |> peek |> should equal 4
    vector |> pop |> pop |> peek |> should equal 1

    vector |> count |> should equal 3
    vector |> pop |> count |> should equal 2
    vector |> pop |> pop |> count |> should equal 1

[<Test>]
let ``vector with 300 elements should allow pop``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    for i in 1..300 do
        vector := pop (!vector)

    !vector |> Seq.toList |> should equal []

[<Test>]
let ``vector with 3 elements can compute hashcodes``() =
    let vector1 = ref empty
    for i in 1..3 do
        vector1 := conj i (!vector1)

    let vector2 = ref empty
    for i in 1..3 do
        vector2 := conj i (!vector2)

    let vector3 = ref empty
    for i in 1..3 do
        vector3 := conj (2*i) (!vector3)

    vector1.GetHashCode() |> should equal (vector2.GetHashCode())
    vector1.GetHashCode() |> should equal (vector2.GetHashCode())

[<Test>]
let ``vector with 3 elements can be compared``() =
    let vector1 = ref empty
    for i in 1..3 do
        vector1 := conj i (!vector1)

    let vector2 = ref empty
    for i in 1..3 do
        vector2 := conj i (!vector2)

    let vector3 = ref empty
    for i in 1..3 do
        vector3 := conj (2*i) (!vector3)


    vector1 = vector1 |> should equal true
    vector1 = vector2 |> should equal true
    vector1 = vector3 |> should equal false

[<Test>]
let ``vector should allow map``() =
    let vector = ref empty
    for i in 1..300 do
        vector := conj i (!vector)

    let vector2 = map (fun x -> x * 2) (!vector)

    let a = vector2 |> Seq.toArray 
    for i in 1..300 do i * 2 |> should equal a.[i-1]

[<Test>]
let ``vector should allow init``() =
    let vector = init 5 (fun x -> x * 2) 
    let s = Seq.init 5 (fun x -> x * 2)

    s |> Seq.toList |> should equal [0;2;4;6;8]
    vector |> Seq.toList |> should equal [0;2;4;6;8]