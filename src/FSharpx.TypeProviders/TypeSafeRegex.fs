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
        let r = Regex(pattern, options)
                
        let matchTy = 
            runtimeType<Match> "MatchType" 
                |> hideOldMethods        
        
        // Add group properties to match type
        for group in r.GetGroupNames() do
            // ignore the group named 0, which represents all input
            if group <> "0" then
                let prop = 
                    ProvidedProperty(
                        propertyName = group, 
                        propertyType = typeof<Group>, 
                        GetterCode = fun args -> <@@ (%%args.[0]:Match).Groups.[group] @@>)
                    |> addXmlDoc(sprintf @"Gets the ""%s"" group from this match" group)
                matchTy.AddMember(prop)
        
        erasedType<Regex> thisAssembly rootNamespace typeName 
            |> hideOldMethods
            |> addXmlDoc "A strongly typed interface to the regular expression '%s'"
            |> addMember (            
                ProvidedMethod(
                    methodName = "IsMatch", 
                    parameters = [ProvidedParameter("input", typeof<string>)], 
                    returnType = typeof<bool>, 
                    IsStaticMethod = true,
                    InvokeCode = fun args -> <@@ Regex.IsMatch(%%args.[0], pattern, options) @@>) 
                |> addXmlDoc "Indicates whether the regular expression finds a match in the specified input string")
            |> addMember matchTy
            |> addMember (
                ProvidedMethod(
                    methodName = "Match", 
                    parameters = [ProvidedParameter("input", typeof<string>)], 
                    returnType = matchTy, 
                    InvokeCode = fun args -> <@@ (%%args.[0]:Regex).Match(%%args.[1]) @@>)
                |> addXmlDoc "Searches the specified input string for the first occurence of this regular expression")
            |> addMember (
                ProvidedConstructor(
                    parameters = [], 
                    InvokeCode = fun args -> <@@ Regex(pattern, options) @@>)
                |> addXmlDoc "Initializes a regular expression instance")
        | _ -> failwith "unexpected parameter values"))