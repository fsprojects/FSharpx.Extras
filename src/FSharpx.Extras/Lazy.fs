// First version copied from the F# Power Pack 
// https://raw.github.com/fsharp/powerpack/master/src/FSharp.PowerPack/Lazy.fs

// (c) Microsoft Corporation 2005-2009.  

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpx.Lazy

let force (x: Lazy<'T>) = x.Force()
