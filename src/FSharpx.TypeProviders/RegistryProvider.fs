module FSharpx.TypeProviders.RegistryProvider

open Samples.FSharpPreviewRelease2011.ProvidedTypes
open FSharpx.TypeProviders.Settings
open FSharpx.TypeProviders.DSL
open Microsoft.Win32

let getAccessibleSubkeys (registryKey:RegistryKey) =
    registryKey.GetSubKeyNames()            
        |> Seq.choose (fun name -> 
            try
               Some (registryKey.OpenSubKey name,name)
            with
            | enx -> None) // TODO: Handle access violation

let rec createRegistryNode (registryKey:RegistryKey,subkeyName) () =     
    runtimeType<obj> subkeyName
        |> hideOldMethods
        |> addXmlDoc (sprintf "A strongly typed interface to '%s'" registryKey.Name)
        |+> (fun () ->
                literalField "Path" registryKey.Name
                    |> addXmlDoc (sprintf "Full path to '%s'" registryKey.Name)) 
        |> addMembersDelayed (
            registryKey
            |> getAccessibleSubkeys 
            |> Seq.map createRegistryNode)

let subNodes = 
    [Registry.ClassesRoot; Registry.CurrentConfig; Registry.CurrentUser; 
     Registry.LocalMachine; Registry.PerformanceData; Registry.Users]
       |> Seq.map (fun key -> key,key.Name)

let typedRegistry =
    erasedType<obj> thisAssembly rootNamespace "RegistryTyped"
      |> addMembersDelayed (Seq.map createRegistryNode subNodes)