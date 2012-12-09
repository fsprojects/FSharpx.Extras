#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif

open System
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.RegistryProvider
open FSharpx.TypeProviders.FilesTypeProvider

generate (fun _ _ -> createTypedFileSystem()) "" [| @"C:\Users\" |] 
|> prettyPrintWithMaxDepth 2
|> Console.WriteLine

generate (fun _ _ -> createTypedRegistry()) "" [||] 
|> prettyPrintWithMaxDepthAndExclusions 2 ["HKEY_CLASSES_ROOT"]
|> Console.WriteLine
