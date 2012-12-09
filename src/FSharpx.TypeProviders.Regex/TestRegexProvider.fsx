#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif

open System
open FSharpx.TypeProviders.Helper
open FSharpx.TypeProviders.RegexTypeProvider

generate (fun _ _ -> typedRegex()) "" [| @"(?<AreaCode>^\d{3})-(?<PhoneNumber>\d{3}-\d{4}$)" |] 
|> prettyPrint 
|> Console.WriteLine
