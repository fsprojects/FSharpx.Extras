#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif

open System
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.VectorTypeProvider

generate (fun _ _ -> vectorTypeProvider) "" [| "X"; "Y"; missingValue; missingValue; missingValue; missingValue; missingValue|] 
|> prettyPrint 
|> Console.WriteLine

generate (fun _ _ -> vectorTypeProvider) "" [| "A"; "B"; "C"; missingValue; missingValue; missingValue; missingValue|] 
|> prettyPrint 
|> Console.WriteLine
