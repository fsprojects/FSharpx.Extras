module internal FSharpx.TypeProviders.CompilerWrapper

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Compiler.SimpleSourceCodeServices

let private (++) a b = Path.Combine(a,b)
let private referenceAssembliesPath = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86) ++ "Reference Assemblies" ++ "Microsoft" 
let private dotNetReferenceAssembliesPath = referenceAssembliesPath ++ "Framework" ++ ".NETFramework" ++ "v4.5"
let private fsharpReferenceAssembliesPath = referenceAssembliesPath ++ "FSharp" ++ "3.0" ++ "Runtime" ++ "v4.0"

AppDomain.CurrentDomain.add_ReflectionOnlyAssemblyResolve (fun _ args ->
    try 
        let assemName = AssemblyName(args.Name)
        if assemName.Name = "FSharp.Core" && assemName.Version.ToString() = "4.3.0.0" then 
            Assembly.ReflectionOnlyLoadFrom (fsharpReferenceAssembliesPath ++ "FSharp.Core.dll")
        else 
            null
    with e ->  
        null)
   
let private services = new SimpleSourceCodeServices()

let getRecords fileName = 
    let tempFile = Path.GetTempPath() + Guid.NewGuid().ToString() + ".dll";
    let argv = [| "fsc.exe"
                  "--target:library"
                  "-o"
                  tempFile              
                  "--noframework"; 
                  "-r:" + fsharpReferenceAssembliesPath ++ "FSharp.Core.dll" 
                  "-r:" + dotNetReferenceAssembliesPath ++ "System.dll"
                  "-r:" + dotNetReferenceAssembliesPath ++ "System.Numerics.dll"
                  fileName |]
    match services.Compile argv with 
    | [||], _ -> 
        let asm = Assembly.ReflectionOnlyLoadFrom tempFile
        asm.GetTypes() |> Array.filter FSharpType.IsRecord |> Some
    | _ -> None
