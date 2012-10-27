/// Starting to implement some helpers on top of ProvidedTypes API
module internal FSharpx.TypeProviders.Helper

open System.IO
open FSharpx.Strings
open Samples.FSharp.ProvidedTypes

let findConfigFile resolutionFolder configFileName =
    if Path.IsPathRooted configFileName then 
        configFileName 
    else 
        Path.Combine(resolutionFolder, configFileName)

let erasedType<'a> assemblyName rootNamespace typeName = 
    ProvidedTypeDefinition(assemblyName, rootNamespace, typeName, Some(typeof<'a>))

let runtimeType<'a> typeName = ProvidedTypeDefinition(niceName typeName, Some typeof<'a>)

let seqType ty = typedefof<seq<_>>.MakeGenericType[| ty |]
let listType ty = typedefof<list<_>>.MakeGenericType[| ty |]
let optionType ty = typedefof<option<_>>.MakeGenericType[| ty |]

/// Implements invalidation of schema when the file changes
let watchForChanges (ownerType:TypeProviderForNamespaces) (fileName:string) = 
    if fileName.StartsWith("http", System.StringComparison.InvariantCultureIgnoreCase) then () else

    let path = Path.GetDirectoryName(fileName)
    let name = Path.GetFileName(fileName)
    let watcher = new FileSystemWatcher(Filter = name, Path = path)
    watcher.Changed.Add(fun _ -> ownerType.Invalidate())
    watcher.EnableRaisingEvents <- true

// Get the assembly and namespace used to house the provided types
let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly()
let rootNamespace = "FSharpx"