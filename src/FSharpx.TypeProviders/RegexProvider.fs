// This typeprovider is originally from the FSharp preview samples

module FSharpx.TypeProviders.RegexTypeProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL

let regexTy = 
    erasedType<Regex> thisAssembly rootNamespace "Regex"
    |> staticParameters
        ["pattern", typeof<string>, None]
        (fun typeName parameterValues ->

            match parameterValues with 
            | [| :? string as pattern |] ->

            let groupProperties =
                Regex(pattern).GetGroupNames()
                    |> Seq.map (fun group ->
                            provideProperty 
                                (if group <> "0" then group else "CompleteMatch")
                                typeof<Group>
                                (fun args -> <@@ (%%args.[0]:Match).Groups.[group] @@>)
                                |> addXmlDoc(sprintf @"Gets the ""%s"" group from this match" group))
                
            let matchType =
                runtimeType<Match> "MatchType"
                  |> hideOldMethods
                  |++!> groupProperties

            erasedType<Regex> thisAssembly rootNamespace typeName 
                |> hideOldMethods
                |> addXmlDoc "A strongly typed interface to the regular expression '%s'"
                |+!> (provideMethod
                        "IsMatch"
                        ["input", typeof<string>]
                        typeof<bool>
                        (fun args -> <@@ Regex.IsMatch(%%args.[0], pattern) @@>)
                    |> makeStatic
                    |> addXmlDoc "Indicates whether the regular expression finds a match in the specified input string")
                |+!> matchType
                |+!> (provideMethod 
                        "Match"
                        ["input", typeof<string>]
                        matchType
                        (fun args -> <@@ (%%args.[0]:Regex).Match(%%args.[1]) @@>)
                    |> addXmlDoc "Searches the specified input string for the first occurence of this regular expression")
                |+!> (provideConstructor
                        [] 
                        (fun args -> <@@ Regex(pattern) @@>)
                    |> addXmlDoc "Initializes a regular expression instance")
            | _ -> failwith "unexpected parameter values")