// Originally ported from http://fsharp3sample.codeplex.com
module FSharpx.TypeProviders.StateMachineProvider

open System.Xml
open System.Xml.Linq

type IState = interface
    abstract member EnterFunction : unit -> unit
    abstract member ExitFunction : unit->unit
end

// define a node record
type Node = { Name : string; NextNodes : (string option * Node) list}

// define DGML DU
type DGML = 
    | Node of string
    | Link of string * string * (string option)


// define DGML class
type DGMLClass() = class   
    let mutable nodes = Unchecked.defaultof<Node list>
    let mutable links = Unchecked.defaultof<DGML list>    
    let mutable currentState = System.String.Empty

    // current state
    member this.CurrentState 
        with get() = currentState
        and private set(v) = currentState <- v

    // all links in the DGML file
    member this.Links 
        with get() = links
        and private set(v) = links <- v

    // all nodes in the DGML file
    member this.Nodes 
        with get() = nodes
        and private set(v) = nodes <- v              

    // initialize the state machien from fileName and set the initial state to initState
    member this.Init(fileName:string, initState) = 
        let file = XDocument.Load(fileName, LoadOptions.None)
        this.Links <- 
            file.Descendants() 
            |> Seq.filter (fun node -> node.Name.LocalName = "Link")
            |> Seq.map (fun node -> 
                            let sourceName = node.Attribute(XName.Get("Source")).Value
                            let targetName = node.Attribute(XName.Get("Target")).Value                            
                            let label = 
                              let attr = node.Attribute(XName.Get("Label"))
                              if attr = null then None else Some(attr.Value)
                            DGML.Link(sourceName, targetName,label))
            |> Seq.toList

        let getNextNodes fromNodeName= 
                this.Links 
                |> Seq.filter (fun (Link(a, b, l)) -> a = fromNodeName)
                |> Seq.map (fun (Link(a,b,l)) -> l,this.FindNode(b))
                |> Seq.filter (fun (_,n) -> match n with Some(x) -> true | None -> false)
                |> Seq.map (fun (l,Some(n)) -> l,n)
                |> Seq.toList

        this.Nodes <-
            file.Descendants() 
            |> Seq.filter (fun node -> node.Name.LocalName = "Node")
            |> Seq.map (fun node -> DGML.Node( node.Attribute(XName.Get("Id")).Value) )
            |> Seq.map (fun (Node(n)) -> { Node.Name=n; NextNodes = [] })
            |> Seq.toList
        
        this.Nodes <-    
            this.Nodes
            |> Seq.map (fun n -> { n with NextNodes = (getNextNodes n.Name) } )  
            |> Seq.toList

        this.CurrentState <- initState

    // fine the node by given nodeName
    member this.FindNode(nodeName) : Node option= 
        let result = 
            this.Nodes 
            |> Seq.filter (fun n -> n.Name = nodeName)
        if result |> Seq.isEmpty then None
        else result |> Seq.head |> Some    

    // current node
    member this.CurrentNode
        with get() = 
            this.Nodes 
            |> Seq.filter (fun n -> n.Name = this.CurrentState)
            |> Seq.head
            
    // determine if can transit to a node represented by the nodeName
    member this.CanTransitTo(nodeName:string) =
        this.CurrentNode.NextNodes |> Seq.exists (fun (_,n) -> n.Name = nodeName)

    // force current state to a new state
    member this.ForceStateTo(args) = 
        this.CurrentState <- args        
end

// state machine class which inherit the DGML class
// and use a MailboxProcessor to perform asynchronous message processing (if makeAsync = true)
type StateMachine(makeAsync) as this = class
    inherit DGMLClass()

    let functions = System.Collections.Generic.Dictionary<string, IState>()
    let transit newState =
        if this.CanTransitTo newState then
          this.InvokeExit(this.CurrentNode.Name)
          this.ForceStateTo newState
          this.InvokeEnter newState

    let processor = new MailboxProcessor<string>(fun inbox ->
                    let rec loop () = 
                        async {
                            let! newState = inbox.Receive()
                            transit newState
                            return! loop ()
                        }
                    loop ())    

    do 
        if makeAsync then processor.Start()

    // define the second constructor taking the file name and initial state name
    new(fileName, makeAsync, initState) as secondCtor = 
        new StateMachine(makeAsync)
        then
            secondCtor.Init(fileName, initState)

    // asynchronously or synchronously transit to a new state
    member this.TransitTo(state) = 
        if makeAsync then processor.Post state else transit state

    // set the transition function
    member this.SetFunction(name:string, state:IState) = 
        if functions.ContainsKey(name) then 
            functions.[name] <- state
        else
            functions.Add(name, state)

    // invoke the Exit function
    member private this.InvokeExit(name:string) = 
        if functions.ContainsKey(name) then
            functions.[name].ExitFunction()

    // invoke the Enter function
    member private this.InvokeEnter(name:string) = 
        if functions.ContainsKey(name) then
            functions.[name].EnterFunction()
end

open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

let stateMachineTy ownerType makeAsync (cfg:TypeProviderConfig) =
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
                                        |> addXmlDoc ("Status " + n.Name)))
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
                    |> addXmlDoc "Sets the functions for status changes")
                |+!> (provideConstructor
                        [] 
                        (fun args -> <@@ StateMachine(dgml,makeAsync,initState) @@>)
                    |> addXmlDoc "Initializes a state machine instance"))

type State = { Name:string }

let graph ownerType (cfg:TypeProviderConfig) =
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