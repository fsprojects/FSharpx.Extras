module FSharp.TypeProviders.Tests.FileSystemTests

open FSharpx

type T = FileTyped< @"C:\Users\sforkmann\Documents">

//printf "Dir path = %s" T.Visual_Studio_2010.Code_Snippets.Nemerle.My_Code_Snippets.Path

let printType() = printfn "%A" T.Path