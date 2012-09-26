// Originally ported from http://fsharp3sample.codeplex.com
module FSharpx.TypeProviders.StateMachineProvider

open FSharpx.StateMachine
open FSharpx.TypeProviders.DSL
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

let internal stateMachineTy ownerType makeAsync (cfg:TypeProviderConfig) =
    erasedType<StateMachine> thisAssembly rootNamespace (if makeAsync then "AsyncStateMachine" else "StateMachine")
    |> staticParameters
        ["dgml file name", typeof<string>, None
         "init state", typeof<string>, None]    
        (fun typeName parameterValues -> 
            match parameterValues with 
            | [| :? string as fileName; :? string as initState |] ->

                let dgml = System.IO.Path.Combine(cfg.ResolutionFolder, fileName)

                let stateMachine = StateMachine(makeAsync)
                stateMachine.Init(dgml, initState)                       
                
                watchForChanges ownerType dgml

                erasedType<StateMachine> thisAssembly rootNamespace typeName
                |> hideOldMethods
                |> addXmlDoc "A strongly typed interface to the state machine described in '%s'"
                |++!> (stateMachine.Nodes
                        |> Seq.map (fun n ->
                                    let name = n.Name
                                    provideProperty
                                        name
                                        typeof<string>                                        
                                        (fun args -> <@@ name @@>)
                                        |> addPropertyXmlDoc ("Status " + n.Name)))
                |++!> (stateMachine.Nodes
                        |> Seq.map (fun n ->
                                    let name = n.Name
                                    provideMethod
                                        (sprintf "TransitTo%s" name)
                                        []
                                        typeof<unit>
                                        (fun args -> <@@ (%%args.[0] :> StateMachine).TransitTo(name) @@>)))
                |+!> (provideMethod
                        "SetTransitionFunction"
                        ["Name", typeof<string>
                         "StateClass", typeof<IState>]
                        typeof<unit>
                        (fun args -> <@@ (%%args.[0] :> StateMachine).SetFunction(%%args.[1] :> string, %%args.[2] :> IState) @@>)
                    |> addMethodXmlDoc "Sets the functions for status changes")
                |+!> (provideConstructor
                        [] 
                        (fun args -> <@@ StateMachine(dgml,makeAsync,initState) @@>)
                    |> addConstructorXmlDoc "Initializes a state machine instance"))

let internal graph ownerType (cfg:TypeProviderConfig) =
    erasedType<obj> thisAssembly rootNamespace ("Graph")
    |> staticParameters
        ["dgml file name", typeof<string>, None
         "init state", typeof<string>, None]    
        (fun typeName parameterValues -> 
            match parameterValues with 
            | [| :? string as fileName; :? string as initState |] ->                
                let dgml = System.IO.Path.Combine(cfg.ResolutionFolder, fileName)
                watchForChanges ownerType dgml

                let ownerType = erasedType<obj> thisAssembly rootNamespace typeName 
                
                let stateMachine = StateMachine(false)
                stateMachine.Init(dgml, initState)      
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

                        states.[node.Name]
                        |+!>
                          (provideMethod
                                trasitionName
                                []
                                states.[name]
                                (fun args -> <@@ { Name = name } @@>))
                        |> ignore
                
                let initalState =
                    match states.TryGetValue initState with
                    | true,v -> v
                    | _ -> failwithf "The initial state \"%s\" is not part of the state machine." initState

                ownerType
                |> addXmlDoc "A strongly typed interface to the state machine described in '%s'"
                |+!> (provideProperty
                        "InitialState"
                        initalState
                        (fun args -> <@@  { Name = initState } @@>)
                          |> makePropertyStatic))

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