// The original idea for this typeprovider is from Ivan Towlson
// http://www.mindscapehq.com/blog/index.php/2011/09/19/f-type-providers-as-if-by-magic/

module FSharpx.TypeProviders.VectorTypeProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL

let dotProduct = Array.fold2 (fun acc x y -> acc + x * y) 0.
let add = Array.map2 (fun x y -> x + y)
let subtract = Array.map2 (fun x y -> x - y)
    
let vectorTy =
    let missingValue = "@@@missingValue###"
    erasedType<obj> thisAssembly rootNamespace "Vector"
    |> staticParameters 
          [ for p in 1..7 -> "axis" + p.ToString() , typeof<string>, Some(missingValue :> obj) ]
          (fun typeName parameterValues ->
                let parameters = parameterValues |> Seq.map string |> Seq.filter ((<>) missingValue) |> List.ofSeq
                let dimensions = parameters |> List.length
                let newType = 
                    erasedType<float array> thisAssembly rootNamespace typeName
                    |> hideOldMethods
                newType
                    |+!> (provideConstructor
                            (parameters |> List.map (fun p -> p, typeof<float>))
                            (fun args ->              
                                match dimensions with
                                | 0 -> <@@ [||] @@>
                                | 1 -> <@@ [|(%%args.[0]:float)|] @@>
                                | 2 -> <@@ [|(%%args.[0]:float);(%%args.[1]:float)|] @@>
                                | 3 -> <@@ [|(%%args.[0]:float);(%%args.[1]:float);(%%args.[2]:float)|] @@>
                                | 4 -> <@@ [|(%%args.[0]:float);(%%args.[1]:float);(%%args.[2]:float);(%%args.[3]:float)|] @@>
                                | 5 -> <@@ [|(%%args.[0]:float);(%%args.[1]:float);(%%args.[2]:float);(%%args.[3]:float);(%%args.[4]:float)|] @@>
                                | 6 -> <@@ [|(%%args.[0]:float);(%%args.[1]:float);(%%args.[2]:float);(%%args.[3]:float);(%%args.[4]:float);(%%args.[5]:float)|] @@>
                                | _ -> <@@ [|(%%args.[0]:float);(%%args.[1]:float);(%%args.[2]:float);(%%args.[3]:float);(%%args.[4]:float);(%%args.[5]:float);(%%args.[6]:float)|] @@>)
                           |> addXmlDoc "Initializes a vector instance")
                    |+!> (provideMethod
                            "DotProduct"
                            ["factor", newType]
                            typeof<float>
                            (fun args -> <@@ dotProduct (%%args.[0]:float array) (%%args.[1]:float array) @@>)
                           |> addXmlDoc "Calculates the dot product with the given factor.")
                    |+!> (provideMethod
                            "Add"
                            ["summand", newType]
                            newType
                            (fun args -> <@@ add (%%args.[0]:float array) (%%args.[1]:float array) @@>)
                           |> addXmlDoc "Calculates the sum with the given summand.")
                    |+!> (provideMethod
                            "Subtract"
                            ["subtrahend", newType]
                            newType
                            (fun args -> <@@ subtract (%%args.[0]:float array) (%%args.[1]:float array) @@>)
                           |> addXmlDoc "Calculates the difference with the given subtrahend.")
                    |++!> (parameters
                            |> Seq.mapi (fun i name ->
                                    provideProperty 
                                        name
                                        typeof<float>
                                        (fun args -> <@@ (%%args.[0]:float array).[i] @@>)
                                      |> addXmlDoc (sprintf @"Gets the %s axis." name))))