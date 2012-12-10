// The original idea for this typeprovider is from Ivan Towlson
// http://www.mindscapehq.com/blog/index.php/2011/09/19/f-type-providers-as-if-by-magic/
module FSharpx.TypeProviders.VectorTypeProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Helper

let dotProduct x y : float = Array.map2 (*) x y |> Array.sum
let add x y : float[] = Array.map2 (+) x y
let scale x factor : float[] = Array.map ((*) factor) x
let subtract x y : float[] = Array.map2 (-) x y
   
let internal vectorTypeProvider =
    let vectorType = erasedType<obj> thisAssembly rootNamespace "Vector"
    vectorType.DefineStaticParameters(
        parameters = [ for p in 1..7 -> ProvidedStaticParameter("axis" + p.ToString(), typeof<string>, missingValue)], 
        instantiationFunction = (fun typeName parameterValues ->
            let parameters = parameterValues |> Seq.map string |> Seq.filter ((<>) missingValue) |> List.ofSeq
            let dimensions = parameters |> List.length
            let vectorType = erasedType<float array> thisAssembly rootNamespace typeName

            vectorType.HideObjectMethods <- true

            let ctor = 
                ProvidedConstructor(
                    parameters = (parameters |> List.map (fun p -> ProvidedParameter(p, typeof<float>))), 
                    InvokeCode = (fun args -> Quotations.Expr.NewArray(typeof<float>,args)))

            ctor.AddXmlDoc "Initializes a vector instance"
            vectorType.AddMember ctor


            let dotProduct =
                ProvidedMethod(
                    methodName = "DotProduct",
                    parameters = [ProvidedParameter("factor", vectorType)],
                    returnType = typeof<float>,
                    InvokeCode = (fun [this; other] -> <@@ dotProduct %%this %%other @@>))
            dotProduct.AddXmlDoc "Calculates the dot product with the given factor."

            vectorType.AddMember dotProduct


            let scale =
                ProvidedMethod(
                    methodName = "Scale",
                    parameters = [ProvidedParameter("factor", typeof<float>)],
                    returnType = vectorType,
                    InvokeCode = (fun [this; factor] -> <@@ scale %%this %%factor @@>))
            scale.AddXmlDoc "Calculates the scalar multiplication with the given factor."

            vectorType.AddMember scale

            let add =
                ProvidedMethod(
                    methodName = "Add",
                    parameters = [ProvidedParameter("summand", vectorType)],
                    returnType = vectorType,
                    InvokeCode = (fun [this; other] -> <@@ add %%this %%other @@>))
            add.AddXmlDoc "Calculates the sum with the given summand."

            vectorType.AddMember add

            let subtract =
                ProvidedMethod(
                    methodName = "Subtract",
                    parameters = [ProvidedParameter("subtrahend", vectorType)],
                    returnType = vectorType,
                    InvokeCode = (fun [this; other] -> <@@ subtract %%this %%other @@>))
            subtract.AddXmlDoc "Calculates the difference with the given subtrahend."

            vectorType.AddMember subtract

            let equals =
                ProvidedMethod(
                    methodName = "Equals",
                    parameters = [ProvidedParameter("other", vectorType)],
                    returnType = typeof<bool>,
                    InvokeCode = (fun [this; other] -> <@@ (%%this:float[]) = %%other @@>))
            equals.AddXmlDoc "Returns wether the given objects are equal."

            vectorType.AddMember equals

            parameters
                |> Seq.iteri (fun i parameter ->
                                let property =
                                    ProvidedProperty(
                                        propertyName = parameter,
                                        propertyType = typeof<float>,
                                        GetterCode = (fun args -> <@@ (%%args.[0]:float array).[i] @@>))
                                property.AddXmlDoc(sprintf @"Gets the %s axis." parameter)
                                vectorType.AddMember property)

            vectorType))
    vectorType

[<TypeProvider>]
type public MathProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(rootNamespace,[vectorTypeProvider])

[<TypeProviderAssembly>]
do ()