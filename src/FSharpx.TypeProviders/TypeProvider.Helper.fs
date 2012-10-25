/// Starting to implement some helpers on top of ProvidedTypes API
module internal FSharpx.TypeProviders.Helper

open System.IO
open FSharpx.Strings
open Samples.FSharp.ProvidedTypes

type FilePosition =  
   { Line: int; 
     Column: int;
     FileName: string }

let findConfigFile resolutionFolder configFileName =
    if Path.IsPathRooted configFileName then 
        configFileName 
    else 
        Path.Combine(resolutionFolder, configFileName)

let eraseType assemblyName rootNamespace typeName toType = 
    ProvidedTypeDefinition(assemblyName, rootNamespace, typeName, Some toType)

let erasedType<'a> assemblyName rootNamespace typeName = 
    eraseType assemblyName rootNamespace typeName typeof<'a>

let runtimeType<'a> typeName = ProvidedTypeDefinition(niceName typeName, Some typeof<'a>)

/// Implements invalidation of schema when the file changes
let watchForChanges (ownerType:TypeProviderForNamespaces) (fileName:string) = 
    if not (fileName.StartsWith("http", System.StringComparison.InvariantCultureIgnoreCase)) then
      let path = Path.GetDirectoryName(fileName)
      let name = Path.GetFileName(fileName)
      let watcher = new FileSystemWatcher(Filter = name, Path = path)
      watcher.Changed.Add(fun _ ->
        ownerType.Invalidate()) 
      watcher.EnableRaisingEvents <- true   

// Get the assembly and namespace used to house the provided types
let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly()
let rootNamespace = "FSharpx"