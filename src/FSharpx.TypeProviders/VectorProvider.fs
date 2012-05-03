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
let scale a factor = Array.map (fun x -> factor * x) a
let subtract = Array.map2 (fun x y -> x - y)
let equals x y = Array.fold2 (fun acc x y -> acc && x = y) true x y
   
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
                            (fun args -> Quotations.Expr.NewArray(typeof<float>,args))
                           |> addXmlDoc "Initializes a vector instance")
                    |+!> (provideMethod
                            "DotProduct"
                            ["factor", newType]
                            typeof<float>
                            (fun args -> <@@ dotProduct (%%args.[0]:float array) (%%args.[1]:float array) @@>)
                           |> addXmlDoc "Calculates the dot product with the given factor.")
                    |+!> (provideMethod
                            "Scale"
                            ["factor", typeof<float>]
                            newType
                            (fun args -> <@@ scale (%%args.[0]:float array) (%%args.[1]:float) @@>)
                           |> addXmlDoc "Calculates the scalar multiplication with the given factor.")
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
                    |+!> (provideMethod
                            "Equals"
                            ["other", newType]
                            typeof<bool>
                            (fun args -> <@@ equals (%%args.[0]:float array) (%%args.[1]:float array) @@>)
                           |> addXmlDoc "Returns wether the given objects are equal.")
                    |++!> (parameters
                            |> Seq.mapi (fun i name ->
                                    provideProperty 
                                        name
                                        typeof<float>
                                        (fun args -> <@@ (%%args.[0]:float array).[i] @@>)
                                      |> addXmlDoc (sprintf @"Gets the %s axis." name))))