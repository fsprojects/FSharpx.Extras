// This typeprovider is originally from the FSharp preview samples

module FSharpx.TypeProviders.RegexTypeProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Settings

let baseTy = typeof<obj>
let regexTy = ProvidedTypeDefinition(thisAssembly, rootNamespace, "RegexTyped", Some baseTy)

do regexTy.DefineStaticParameters(
    parameters=
        [ProvidedStaticParameter("pattern", typeof<string>)
         ProvidedStaticParameter("options", typeof<RegexOptions>, RegexOptions.None)], 
    instantiationFunction=(fun typeName parameterValues ->

        match parameterValues with 
        | [| :? string as pattern; :? RegexOptions as options |] -> 
        // Create an instance of the regular expression. 
        //
        // This will fail with System.ArgumentException if the regular expression is invalid. 
        // The exception will excape the type provider and be reported in client code.
        let r = System.Text.RegularExpressions.Regex(pattern, options)            

        // Declare the typed regex provided type.
        // The type erasure of this typs ia 'obj', even though the representation will always be a Regex
        // This, combined with hiding the object methods, makes the IntelliSense experience simpler.
        let ty = ProvidedTypeDefinition(
                    assembly = thisAssembly, 
                    namespaceName = rootNamespace, 
                    typeName = typeName, 
                    baseType = Some baseTy, 
                    HideObjectMethods = true)

        ty.AddXmlDoc "A strongly typed interface to the regular expression '%s'"

        // Provide strongly typed version of Regex.IsMatch static method
        let isMatch = ProvidedMethod(
                        methodName = "IsMatch", 
                        parameters = [ProvidedParameter("input", typeof<string>)], 
                        returnType = typeof<bool>, 
                        IsStaticMethod = true,
                        InvokeCode = fun args -> <@@ Regex.IsMatch(%%args.[0], pattern, options) @@>) 

        isMatch.AddXmlDoc "Indicates whether the regular expression finds a match in the specified input string"

        ty.AddMember isMatch

        // Provided type for matches
        // Again, erase to obj even though the representation will always be a Match
        let matchTy = ProvidedTypeDefinition(
                        typeName = "MatchType", 
                        baseType = Some baseTy, 
                        HideObjectMethods = true)

        // Nest the match type within parameterized Regex type
        ty.AddMember matchTy
        
        // Add group properties to match type
        for group in r.GetGroupNames() do
            // ignore the group named 0, which represents all input
            if group <> "0" then
                let prop = ProvidedProperty(
                            propertyName = group, 
                            propertyType = typeof<Group>, 
                            GetterCode = fun args -> <@@ ((%%args.[0]:obj) :?> Match).Groups.[group] @@>)
                prop.AddXmlDoc(sprintf @"Gets the ""%s"" group from this match" group)
                matchTy.AddMember(prop)

        // Provide strongly typed version of Regex.Match instance method
        let matchMeth = ProvidedMethod(
                            methodName = "Match", 
                            parameters = [ProvidedParameter("input", typeof<string>)], 
                            returnType = matchTy, 
                            InvokeCode = fun args -> <@@ ((%%args.[0]:obj) :?> Regex).Match(%%args.[1]) :> obj @@>)
        matchMeth.AddXmlDoc "Searches the specified input string for the first occurence of this regular expression"
            
        ty.AddMember matchMeth
            
        // Declare a constructor
        let ctor = ProvidedConstructor(
                    parameters = [], 
                    InvokeCode = fun args -> <@@ Regex(pattern, options) :> obj @@>)

        // Add documentation to the constructor
        ctor.AddXmlDoc "Initializes a regular expression instance"

        ty.AddMember ctor
            
        ty
        | _ -> failwith "unexpected parameter values"))    