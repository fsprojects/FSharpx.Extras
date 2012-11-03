module FSharpx.TypeProviders.ProtoBufProvider

open FSharpx
open FSharpx.TypeProviders.Helper
open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Text.RegularExpressions
open System.Collections.Generic

// Create the main provided type
let internal createProtoBufType ownerType (cfg:TypeProviderConfig) =
    let protobufType = erasedType<obj> thisAssembly rootNamespace "ProtoBuf"
    protobufType.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("filename", typeof<string>)], 
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as fileName |] -> 
                let resolvedFileName = findConfigFile cfg.ResolutionFolder fileName
                watchForChanges ownerType resolvedFileName
                 
                let protobufType = erasedType<Dictionary<string,obj>> thisAssembly rootNamespace typeName 
                protobufType.AddXmlDoc(sprintf "A strongly typed interface to the protocol buffer defined in '%s'" fileName)

                let message = ProtoBufParser.parse resolvedFileName
                for field in message.Fields do                    
                    let property = 
                        let name = field.Name
                        let pType = field.Type
                        ProvidedProperty (
                            propertyName = field.Name,
                            propertyType = pType,
                            GetterCode =
                                (match field.Type with
                                 | x when x = typeof<string> -> 
                                     (fun args -> <@@ let dict = (%%args.[0]:Dictionary<string,obj>)
                                                      if dict.ContainsKey name then dict.[name] :?> string else "" @@>)
                                 | x when x = typeof<string option> -> 
                                     (fun args -> <@@ let dict = (%%args.[0]:Dictionary<string,obj>)
                                                      if dict.ContainsKey name then dict.[name] :?> string option else None @@>)
                                 | x when x = typeof<int> -> 
                                     (fun args -> <@@ let dict = (%%args.[0]:Dictionary<string,obj>)
                                                      if dict.ContainsKey name then dict.[name] :?> int else 0 @@>)),
                            SetterCode =
                                (match field.Type with
                                 | x when x = typeof<string> -> 
                                     (fun args -> <@@ (%%args.[0]:Dictionary<string,obj>).[name] <- (%%args.[1]:string) @@>)
                                 | x when x = typeof<string option> -> 
                                     (fun args -> <@@ (%%args.[0]:Dictionary<string,obj>).[name] <- (%%args.[1]:string option) @@>)
                                 | x when x = typeof<int> -> 
                                     (fun args -> <@@ (%%args.[0]:Dictionary<string,obj>).[name] <- (%%args.[1]:int) @@>)))

                    protobufType.AddMember property


                let serializeMethod =
                    ProvidedMethod(
                        methodName = "Serialize",
                        parameters = [ProvidedParameter("Stream",typeof<Stream>)],
                        returnType = typeof<unit>,
                        InvokeCode = (fun args -> <@@ ProtoSerializer.serialize (%%args.[0]:Dictionary<string,obj>) (%%args.[1]:Stream) @@>))

                protobufType.AddMember serializeMethod

                let defaultConstructor = 
                    ProvidedConstructor(
                        parameters = [],
                        InvokeCode = (fun _ -> <@@ new Dictionary<string,obj>()  @@>))
                defaultConstructor.AddXmlDoc "Initializes this instance"

                protobufType.AddMember defaultConstructor

                let streamConstructor =
                    ProvidedConstructor(
                        parameters = [ProvidedParameter("Stream",typeof<Stream>)],
                        InvokeCode = (fun args -> <@@ ProtoSerializer.deserialize (%%args.[0]:Stream) @@>))

                protobufType.AddMember streamConstructor

                protobufType))

    protobufType