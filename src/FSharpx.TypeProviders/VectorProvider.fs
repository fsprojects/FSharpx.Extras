// The original idea for this typeprovider is from Ivan Towlson
// http://www.mindscapehq.com/blog/index.php/2011/09/19/f-type-providers-as-if-by-magic/

module FSharpx.TypeProviders.VectorTypeProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL

let dotProduct x y : float = Array.map2 (*) x y |> Array.sum
let add x y : float[] = Array.map2 (+) x y
let scale x factor : float[] = Array.map ((*) factor) x
let subtract x y : float[] = Array.map2 (-) x y
   
let vectorTypeProvider =
    let missingValue = "@@@missingValue###"
    erasedType<obj> thisAssembly rootNamespace "Vector"
    |> staticParameters 
          [ for p in 1..7 -> "axis" + p.ToString() , typeof<string>, Some(missingValue :> obj) ]
          (fun typeName parameterValues ->
                let parameters = parameterValues |> Seq.map string |> Seq.filter ((<>) missingValue) |> List.ofSeq
                let dimensions = parameters |> List.length
                let vectorType = 
                    erasedType<float array> thisAssembly rootNamespace typeName
                    |> hideOldMethods

                vectorType
                    |+!> (provideConstructor
                            (parameters |> List.map (fun p -> p, typeof<float>))
                            (fun args -> Quotations.Expr.NewArray(typeof<float>,args))
                           |> addXmlDoc "Initializes a vector instance")
                    |+!> (provideMethod
                            "DotProduct"
                            ["factor", vectorType]
                            typeof<float>
                            (fun [this; other] -> <@@ dotProduct %%this %%other @@>)
                           |> addXmlDoc "Calculates the dot product with the given factor.")
                    |+!> (provideMethod
                            "Scale"
                            ["factor", typeof<float>]
                            vectorType
                            (fun [this; factor] -> <@@ scale %%this %%factor @@>)
                           |> addXmlDoc "Calculates the scalar multiplication with the given factor.")
                    |+!> (provideMethod
                            "Add"
                            ["summand", vectorType]
                            vectorType
                            (fun [this; other] -> <@@ add %%this %%other @@>)
                           |> addXmlDoc "Calculates the sum with the given summand.")
                    |+!> (provideMethod
                            "Subtract"
                            ["subtrahend", vectorType]
                            vectorType
                            (fun [this; other] -> <@@ subtract %%this %%other @@>)
                           |> addXmlDoc "Calculates the difference with the given subtrahend.")
                    |+!> (provideMethod
                            "Equals"
                            ["other", vectorType]
                            typeof<bool>
                            (fun [this; other] -> <@@ (%%this:float[]) = %%other @@>)
                           |> addXmlDoc "Returns wether the given objects are equal.")
                    |++!> (parameters
                            |> Seq.mapi (fun i name ->
                                    provideProperty 
                                        name
                                        typeof<float>
                                        (fun args -> <@@ (%%args.[0]:float array).[i] @@>)
                                      |> addXmlDoc (sprintf @"Gets the %s axis." name))))