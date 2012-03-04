module FSharpx.TypeProviders.JsonProvider

open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open System.Reflection
open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Text.RegularExpressions
open FSharpx.Http.JSON
open Microsoft.FSharp.Quotations

let rec annotateAsJson (json:JSON) (ownerTy:ProvidedTypeDefinition) =
    let appendProperty name value = 
        ownerTy
            |> addXmlDoc (sprintf "Property %s" name)
            |> hideOldMethods
            |+> (fun () -> provideProperty name (value.GetType()) (fun args -> <@@ value @@>))
    try        
        match json with
        | Text t -> appendProperty "Text" t
        | Number n -> appendProperty "Number" n
        | Boolean b -> appendProperty "Boolean" b
        | Null -> ownerTy
        | JArray list -> raise <| new System.NotImplementedException()
        | JObject map -> 
            ownerTy
                |> addXmlDoc "JObject property type"
                |> hideOldMethods
                |++!> (
                    map
                        |> Seq.choose (fun e -> 
                                match e.Value with
                                | Text t -> Some(provideProperty e.Key (t.GetType()) (fun args -> <@@ t @@>) :> MemberInfo)
                                | Number n -> Some(provideProperty e.Key (n.GetType()) (fun args -> <@@ n @@>) :> MemberInfo)
                                | Boolean b -> Some(provideProperty e.Key (b.GetType()) (fun args -> <@@ b @@>) :> MemberInfo)
                                | JArray list ->
                                    let newType = annotateAsJson list.[0] (runtimeType<obj> (ownerTy.Name + "_" + e.Key))
                                    ownerTy.AddMember newType
                                    Some(provideProperty
                                            e.Key
                                            newType // TODO: list
                                            (fun args -> Expr.Coerce(<@@ (%%args.[0] : obj) @@>, newType))
                                            :> MemberInfo)
                                | JObject map ->                                    
                                    let newType = annotateAsJson e.Value (runtimeType<obj> (ownerTy.Name + "_" + e.Key))
                                    ownerTy.AddMember newType

                                    Some(provideProperty
                                        e.Key
                                        newType
                                        (fun args -> Expr.Coerce(<@@ (%%args.[0] : obj) @@>, newType))
                                        :> MemberInfo)
                                | _ -> None))
    with
    | exn -> ownerTy

// Create the main provided type
let jsonType (cfg:TypeProviderConfig) =
    erasedType<obj> thisAssembly rootNamespace "JSON"
     |> staticParameter "json" (fun typeName (json:string) -> 
            annotateAsJson
              (parse json)
              (erasedType<obj> thisAssembly rootNamespace typeName)
            |+!> (provideConstructor [] (fun args -> <@@ "The object data" :> obj @@>)))