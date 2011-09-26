module FSharp.TypeProviders.Tests.FileSystemTests

open FSharpx

type T = FileSystemTyped< @"C:\Users\Steffen\Documents">

let printType() = printfn "%A" T.Visual_Studio_11.Path