module FSharpx.TypeProviders.DSL

open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Reflection

// Starting to implement a DSL on top of ProvidedTypes API

let cleanupTypeName(name:string) = name.Replace(' ','_')

let hideOldMethods (typeDef:ProvidedTypeDefinition) = 
    typeDef.HideObjectMethods <- true
    typeDef

let runtimeType<'a> typeName = 
    ProvidedTypeDefinition(typeName = cleanupTypeName typeName, baseType = Some typeof<'a>)

let erasedType<'a> assemblyName rootNamespace typeName = 
    ProvidedTypeDefinition(assemblyName, rootNamespace, cleanupTypeName typeName, Some typeof<'a>)

let literalField name xmlDoc (value:'a) =
    let field = ProvidedLiteralField(cleanupTypeName name, typeof<'a>, value)
    field.AddXmlDoc xmlDoc
    field

let addMember memberDef (typeDef:ProvidedTypeDefinition)  =
    typeDef.AddMember memberDef
    typeDef