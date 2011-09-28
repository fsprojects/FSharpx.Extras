// This typeprovider is originally from the FSharp preview samples

module FSharpx.TypeProviders.RegexTypeProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL

let regexTy = erasedType<Regex> thisAssembly rootNamespace "RegexTyped"

regexTy.DefineStaticParameters(
    parameters=
        [ProvidedStaticParameter("pattern", typeof<string>)
         ProvidedStaticParameter("options", typeof<RegexOptions>, RegexOptions.None)], 
    instantiationFunction=(fun typeName parameterValues ->

        match parameterValues with 
        | [| :? string as pattern; :? RegexOptions as options |] ->

        let groupProperties =
            Regex(pattern, options).GetGroupNames()
                |> Seq.map (fun group ->
                        property<Group> 
                            (if group <> "0" then group else "CompleteMatch") 
                            (fun args -> <@@ (%%args.[0]:Match).Groups.[group] @@>)
                            |> addXmlDoc(sprintf @"Gets the ""%s"" group from this match" group))
                
        let matchTy =
            runtimeType<Match> "MatchType"
              |> hideOldMethods 
              |> addMembers groupProperties

        erasedType<Regex> thisAssembly rootNamespace typeName 
            |> hideOldMethods
            |> addXmlDoc "A strongly typed interface to the regular expression '%s'"
            |> addMember (
                provideMethod
                    "IsMatch"
                    ["input", typeof<string>]
                    typeof<bool>
                    (fun args -> <@@ Regex.IsMatch(%%args.[0], pattern, options) @@>)
                |> makeStatic
                |> addXmlDoc "Indicates whether the regular expression finds a match in the specified input string")
            |> addMember matchTy
            |> addMember (
                provideMethod 
                    "Match"
                    ["input", typeof<string>]
                    matchTy
                    (fun args -> <@@ (%%args.[0]:Regex).Match(%%args.[1]) @@>)
                |> addXmlDoc "Searches the specified input string for the first occurence of this regular expression")
            |> addMember (
                ProvidedConstructor(
                    parameters = [], 
                    InvokeCode = fun args -> <@@ Regex(pattern, options) @@>)
                |> addXmlDoc "Initializes a regular expression instance")
        | _ -> failwith "unexpected parameter values"))