#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif

open System
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.RecordZipperProvider

generate recordZipperType __SOURCE_DIRECTORY__ [| "CourseraSample.fs" |] 
|> prettyPrint 
|> Console.WriteLine
