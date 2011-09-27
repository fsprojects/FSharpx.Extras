module FSharp.TypeProviders.Tests.FileSystemTests

open FSharpx

type T = FileSystemTyped< @"C:\Users\Steffen\Documents">

let printPath() = printfn "%s" T.Visual_Studio_2010.Path
let printFile() = printfn "%s" T.``desktop.ini``.Path