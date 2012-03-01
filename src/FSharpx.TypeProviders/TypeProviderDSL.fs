// Starting to implement a DSL on top of ProvidedTypes API
module FSharpx.TypeProviders.DSL

open Samples.FSharp.ProvidedTypes
open System.Reflection
open Microsoft.FSharp.Quotations
open FSharpx.Strings

type FilePosition =  
   { Line: int; 
     Column: int;
     FileName: string }

let fileStart fileName = { Line = 1; Column = 1; FileName = fileName }

/// Converts the given type name proposal into a compilable type name
let cleanupTypeName = replace " " "_"

let hideOldMethods (typeDef:ProvidedTypeDefinition) = 
    typeDef.HideObjectMethods <- true
    typeDef

let inline addXmlDoc xmlDoc (definition: ^a) = 
    (^a : (member AddXmlDoc: string -> unit) (definition,xmlDoc))
    definition

/// Add metadata defining the property's location in the referenced file
let inline addDefinitionLocation (filePosition:FilePosition) (definition: ^a) = 
    if System.String.IsNullOrEmpty filePosition.FileName then definition else
    (^a : (member AddDefinitionLocation: int*int*string -> unit) (definition,filePosition.Line,filePosition.Column,filePosition.FileName))
    definition

let runtimeType<'a> typeName = ProvidedTypeDefinition(cleanupTypeName typeName, Some typeof<'a>)

let eraseType assemblyName rootNamespace typeName toType = 
    ProvidedTypeDefinition(assemblyName, rootNamespace, cleanupTypeName typeName, Some toType)

let erasedType<'a> assemblyName rootNamespace typeName = 
    eraseType assemblyName rootNamespace typeName typeof<'a>

let literalField name (value:'a) =
    ProvidedLiteralField(cleanupTypeName name, typeof<'a>, value)

let inline (|+>) (typeDef:ProvidedTypeDefinition) memberDefinitionF =
    typeDef.AddMemberDelayed memberDefinitionF
    typeDef

let inline (|++>) (typeDef:ProvidedTypeDefinition) memberDefinitionsF =
    Seq.fold (fun typeDef memberF -> typeDef |+> memberF) typeDef memberDefinitionsF

let inline (|+!>) (typeDef:ProvidedTypeDefinition) memberDef =
    typeDef.AddMember memberDef
    typeDef

let inline (|++!>) (typeDef:ProvidedTypeDefinition) memberDef =
    typeDef.AddMembers (memberDef |> Seq.toList)
    typeDef

let provideProperty name propertyType quotationF =    
    ProvidedProperty(
        propertyName = name, 
        propertyType = propertyType, 
        GetterCode = quotationF)

let addSetter quotationF (providedProperty:ProvidedProperty) =
    providedProperty.SetterCode <- quotationF
    providedProperty

let makePropertyStatic (providedProperty:ProvidedProperty) =
    providedProperty.IsStatic <- true
    providedProperty

let provideMethod name parameters returnType quotationF =
    ProvidedMethod(
        methodName = name, 
        parameters = 
            (parameters
                |> Seq.map (fun (name,t) -> ProvidedParameter(name, t)) 
                |> Seq.toList), 
        returnType = returnType, 
        InvokeCode = quotationF)

let provideConstructor parameters quotationF =
    ProvidedConstructor(
        parameters = 
            (parameters
                |> Seq.map (fun (name,t) -> ProvidedParameter(name, t)) 
                |> Seq.toList), 
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

let staticParameters parameters instantiateFunction (typeDef:ProvidedTypeDefinition) =
    typeDef.DefineStaticParameters(
        parameters = 
            (parameters
                |> Seq.map (fun (name,t,initValue) -> 
                        match initValue with
                        | None   -> ProvidedStaticParameter(name, t)
                        | Some v -> ProvidedStaticParameter(name, t,v)) 
                |> Seq.toList), 
        instantiationFunction = instantiateFunction)
    typeDef

open System.IO

let findConfigFile resolutionFolder configFileName =
    if Path.IsPathRooted configFileName then 
        configFileName 
    else 
        Path.Combine(resolutionFolder, configFileName)

let watchPath invalidateF path =
    let folder = Path.GetDirectoryName path
    let file = Path.GetFileName path
    let watcher = new FileSystemWatcher(folder, file)
    watcher.Changed.Add (fun _ -> invalidateF())
    watcher.EnableRaisingEvents <- true