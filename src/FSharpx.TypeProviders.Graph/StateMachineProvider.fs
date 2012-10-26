// Originally ported from http://fsharp3sample.codeplex.com
module FSharpx.TypeProviders.StateMachineProvider

open FSharpx.StateMachine
open FSharpx.Strings
open FSharpx.TypeProviders.Helper
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

let internal stateMachineTy ownerType makeAsync (cfg:TypeProviderConfig) =
    let stateMachineType = erasedType<StateMachine> thisAssembly rootNamespace (if makeAsync then "AsyncStateMachine" else "StateMachine")
    stateMachineType.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("dgml file name", typeof<string>)
                      ProvidedStaticParameter("init state", typeof<string>)],
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as fileName; :? string as initState |] ->

                let dgml = System.IO.Path.Combine(cfg.ResolutionFolder, fileName)

                let stateMachine = StateMachine(makeAsync)
                stateMachine.Init(dgml, initState)                       
                
                watchForChanges ownerType dgml

                let stateMachineType = erasedType<StateMachine> thisAssembly rootNamespace typeName
                stateMachineType.HideObjectMethods <- true
                stateMachineType.AddXmlDoc (sprintf "A strongly typed interface to the state machine described in '%s'" fileName)

                for n in stateMachine.Nodes do
                    let name = n.Name
                    let property =
                        ProvidedProperty(
                            propertyName = niceName name,
                            propertyType = typeof<string>,
                            GetterCode = (fun args -> <@@ name @@>))
                    property.AddXmlDoc("Status " + n.Name)
                    stateMachineType.AddMember property

                    let transitMethod = 
                        ProvidedMethod(
                            methodName = niceName (sprintf "TransitTo%s" name),
                            parameters = [],
                            returnType = typeof<unit>,
                            InvokeCode = (fun args -> <@@ (%%args.[0] :> StateMachine).TransitTo(name) @@>))
                    stateMachineType.AddMember transitMethod

                let setTransitionFunctionMethod =
                    ProvidedMethod(
                        methodName = "SetTransitionFunction",
                        parameters = [ProvidedParameter("Name", typeof<string>)
                                      ProvidedParameter("StateClass", typeof<IState>)],
                        returnType = typeof<unit>,
                        InvokeCode = (fun args -> <@@ (%%args.[0] :> StateMachine).SetFunction(%%args.[1] :> string, %%args.[2] :> IState) @@>))

                setTransitionFunctionMethod.AddXmlDoc "Sets the functions for status changes"
                stateMachineType.AddMember setTransitionFunctionMethod

                let defaultConstructor = 
                    ProvidedConstructor(
                        parameters = [],
                        InvokeCode = (fun _ -> <@@ StateMachine(dgml,makeAsync,initState) @@>))
                defaultConstructor.AddXmlDoc "Initializes a state machine instance"

                stateMachineType.AddMember defaultConstructor
                stateMachineType))
    stateMachineType

let internal graph ownerType (cfg:TypeProviderConfig) =
    let graphType = erasedType<obj> thisAssembly rootNamespace ("Graph")
    graphType.DefineStaticParameters(
        parameters = [ProvidedStaticParameter("dgml file name", typeof<string>)],
        instantiationFunction = (fun typeName parameterValues ->
            match parameterValues with 
            | [| :? string as fileName |] ->                
                let dgml = System.IO.Path.Combine(cfg.ResolutionFolder, fileName)
                watchForChanges ownerType dgml

                let ownerType = erasedType<obj> thisAssembly rootNamespace typeName 
                
                let stateMachine = StateMachine(false)
                stateMachine.Init(dgml, null)      
                let states = System.Collections.Generic.Dictionary<_,_>()

                stateMachine.Nodes
                    |> List.iter (fun node -> states.Add(node.Name,runtimeType<State> node.Name))

                states
                  |> Seq.iter (fun state ->
                                let s = state.Value
                                ownerType.AddMember s)

                for node in stateMachine.Nodes do                    
                    for node2 in node.NextNodes do
                        let label,targetNode = node2
                        let name = targetNode.Name
                        let trasitionName =
                            match label with
                            | Some l -> l
                            | _ -> "TransitTo" + name

                        let transitionMethod =
                            ProvidedMethod(
                                methodName = niceName trasitionName,
                                parameters = [],
                                returnType = states.[name],
                                InvokeCode = (fun args -> <@@ { Name = name } @@>))
                        states.[node.Name].AddMember transitionMethod

                for node1 in stateMachine.Nodes do
                    for targetNode in stateMachine.Nodes do
                        if node1 <> targetNode then
                            match stateMachine.FindShortestPathTo(node1.Name,targetNode.Name) with
                            | Some path ->
                                let name = targetNode.Name
                                let path = path |> List.rev |> List.tail

                                let shortestPathMethod =
                                    ProvidedMethod(
                                        methodName = niceName ("ShortestPathTo" + name),
                                        parameters = [],
                                        returnType = listType typeof<string>,
                                        InvokeCode = (fun args -> <@@ path @@>))
                                states.[node1.Name].AddMember shortestPathMethod
                            | None -> ()

                for node in stateMachine.Nodes do
                    let name = node.Name

                    let startFromMethod =
                        ProvidedMethod(
                            methodName = niceName ("StartFrom" + name),
                            parameters = [],
                            returnType = states.[name],
                            InvokeCode = (fun args -> <@@  { Name = name } @@>),
                            IsStaticMethod = true)
                    ownerType.AddMember startFromMethod

                ownerType))
    graphType

[<TypeProvider>]
type public GraphProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        rootNamespace, 
        [stateMachineTy this true cfg
         stateMachineTy this false cfg
         graph this cfg])

[<TypeProviderAssembly>]
do ()