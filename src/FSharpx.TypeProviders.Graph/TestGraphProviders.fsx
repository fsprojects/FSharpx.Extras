#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif

open System
open System.IO
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.StateMachineProvider

let (++) a b = Path.Combine(a, b)
let resolutionFolder = __SOURCE_DIRECTORY__ ++ ".." ++ ".." ++ "tests" ++ "FSharpx.TypeProviders.Graph.Tests"

generate graph resolutionFolder [| "BorderGatewayProtocol.dgml" |] 
|> prettyPrint 
|> Console.WriteLine

generate graph resolutionFolder [| "Graph1.dgml" |] 
|> prettyPrint 
|> Console.WriteLine

generate graph resolutionFolder [| "Binary.dgml" |] 
|> prettyPrint 
|> Console.WriteLine

generate graph resolutionFolder [| "Turnstile.dgml" |] 
|> prettyPrint 
|> Console.WriteLine

generate (fun owner cfg -> stateMachineTy owner true cfg) resolutionFolder [| "Graph1.dgml"; "State0" |] 
|> prettyPrint 
|> Console.WriteLine

generate (fun owner cfg -> stateMachineTy owner false cfg) resolutionFolder [| "Graph1.dgml"; "State0" |] 
|> prettyPrint 
|> Console.WriteLine

generate graph resolutionFolder [| "Actors.dgml" |] 
|> prettyPrint 
|> Console.WriteLine
