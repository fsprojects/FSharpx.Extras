#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif

open System
open System.IO
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.Freebase.FreebaseTypeExtender

let (++) a b = Path.Combine(a, b)
let runtimeAssembly = __SOURCE_DIRECTORY__ ++ ".." ++ "FSharpx.TypeProviders.Freebase" ++ "bin" ++ "Debug" ++ "FSharpx.TypeProviders.Freebase.dll"

generateWithRuntimeAssembly (fun cfg -> new FreebaseTypeProvider(cfg)) Seq.head runtimeAssembly "" [||] 
|> prettyPrintWithMaxDepth 2
|> Console.WriteLine
