module FSharpx.TypeProviders.DSL

open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Reflection

// Starting to implement a DSL on top of ProvidedTypes API

let cleanupTypeName(name:string) = name.Replace(' ','_')

let hideOldMethods (typeDef:ProvidedTypeDefinition) = 
    typeDef.HideObjectMethods <- true
    typeDef

let inline addXmlDoc xmlDoc (definition: ^a) = 
    (^a : (member AddXmlDoc: string -> unit) (definition,xmlDoc))
    definition

let runtimeType<'a> typeName = 
    ProvidedTypeDefinition(typeName = cleanupTypeName typeName, baseType = Some typeof<'a>)

let erasedType<'a> assemblyName rootNamespace typeName = 
    ProvidedTypeDefinition(assemblyName, rootNamespace, cleanupTypeName typeName, Some typeof<'a>)

let literalField name (value:'a) =
    ProvidedLiteralField(cleanupTypeName name, typeof<'a>, value)

let addMember memberDef (typeDef:ProvidedTypeDefinition) =
    typeDef.AddMember memberDef
    typeDef

let addMembers members ownerType = Seq.fold (fun ownerType subType -> addMember subType ownerType) ownerType members

open Microsoft.FSharp.Quotations

let property<'r> name quotationF =    
    ProvidedProperty(
        propertyName = name, 
        propertyType = typeof<'r>, 
        GetterCode = quotationF)

let provideMethod name parameters returnType quotationF =
    ProvidedMethod(
        methodName = name, 
        parameters = 
            (parameters
                |> Seq.map (fun (name,t) -> ProvidedParameter(name, t)) 
                |> Seq.toList), 
        returnType = returnType, 
        InvokeCode = quotationF)

let makeStatic (providedMethod:ProvidedMethod) =
    providedMethod.IsStaticMethod <- true
    providedMethod
let staticParameter name instantiateFunction (typeDef:ProvidedTypeDefinition) =
    typeDef.DefineStaticParameters(
        parameters = [ProvidedStaticParameter(name, typeof<'a>)], 
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? 'a as parameterValue |] -> instantiateFunction typeName parameterValue
            | x -> failwithf "unexpected parameter values %A" x))
    typeDef