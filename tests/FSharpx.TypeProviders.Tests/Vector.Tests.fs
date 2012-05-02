module FSharpx.TypeProviders.Tests.VectorTests

open NUnit.Framework
open FSharpx
open FsUnit

type Vector2D = Vector<"X","Y">
type Vector3D = Vector<"A","B","C">

[<Test>] 
let ``Can get the typed vector axis from the 2D vector``() =      
    let v = Vector2D(1.,2.5)
    
    v.X |> should equal 1.
    v.Y |> should equal 2.5

[<Test>] 
let ``Can get the typed vector axis from the 3D vector``() =      
    let v = Vector3D(1.1,2.2,3.1)
    
    v.A |> should equal 1.1
    v.B |> should equal 2.2
    v.C |> should equal 3.1

[<Test>] 
let ``Can calc the typed dot product for the 2D vector``() =      
    let v1 = Vector2D(1.,2.)
    let v2 = Vector2D(12.,6.)    
 
    v1.DotProduct(v2) |> should equal 24.

[<Test>] 
let ``Can calc the typed dot product for the 3D vector``() =      
    let v1 = Vector3D(1.,3.,-5.)
    let v2 = Vector3D(4.,-2.,-1.)    
 
    v1.DotProduct(v2) |> should equal 3.

[<Test>] 
let ``Can calc the typed sum for the 3D vector``() =      
    let v1 = Vector3D(1.,3.,-5.)
    let v2 = Vector3D(4.,-2.,-1.)
    let sum = v1.Add(v2)

    sum.A |> should equal 5.
    sum.B |> should equal 1.
    sum.C |> should equal -6.

[<Test>] 
let ``Can calc the typed difference for the 2D vector``() =      
    let v1 = Vector2D(1.,-5.)
    let v2 = Vector2D(4.,-2.2)
    let sum = v1.Subtract(v2)

    sum.X |> should equal -3.
    sum.Y |> should equal -2.8