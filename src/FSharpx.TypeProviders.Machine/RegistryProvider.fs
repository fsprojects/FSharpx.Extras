module internal FSharpx.TypeProviders.RegistryProvider

open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Helper
open Microsoft.Win32

let getAccessibleSubkeys (registryKey:RegistryKey) =
    registryKey.GetSubKeyNames()            
        |> Seq.choose (fun name -> 
            try
                Some (registryKey.OpenSubKey name,name)
            with
            | enx -> None) // TODO: Handle access violation     

let getAccessibleValues (registryKey:RegistryKey) =
    registryKey.GetValueNames()
        |> Seq.filter (System.String.IsNullOrEmpty >> not)
        |> Seq.choose (fun name ->
            try
                Some (registryKey.GetValueKind name,name)
            with
            | enx -> None) // TODO: Handle access violation     

let registryProperty<'a> key valueName = 
    ProvidedProperty(
        propertyName = valueName,
        propertyType = typeof<'a>,
        IsStatic = true,
        GetterCode = (fun args -> <@@ Registry.GetValue(key,valueName,"") :?> 'a @@>),
        SetterCode = (fun args -> <@@ Registry.SetValue(key,valueName,(%%args.[0] : 'a)) @@>))

let rec createRegistryNode (registryKey:RegistryKey,subkeyName) () =   
    let registryNodeType = runtimeType<obj> subkeyName
    registryNodeType.HideObjectMethods <- true
    registryNodeType.AddXmlDoc(sprintf "A strongly typed interface to '%s'" registryKey.Name)
    let pathField = ProvidedLiteralField("Path",typeof<string>,registryKey.Name)
    pathField.AddXmlDoc(sprintf "Full path to '%s'" registryKey.Name)
    registryNodeType.AddMember pathField

    for (kind,name) in getAccessibleValues registryKey do
        let property = 
            match kind with
            // TODO: pattern matching
            | RegistryValueKind.String -> registryProperty<string> registryKey.Name name
            | _ -> registryProperty<obj> registryKey.Name name

        registryNodeType.AddMember property

    for (subkey,name) in getAccessibleSubkeys registryKey do
        registryNodeType.AddMemberDelayed (createRegistryNode(subkey,name))

    registryNodeType

let createTypedRegistry() =
    let registryType = erasedType<obj> thisAssembly rootNamespace "Registry"
    let subNodes = 
        [Registry.ClassesRoot; Registry.CurrentConfig; Registry.CurrentUser; 
         Registry.LocalMachine; Registry.PerformanceData; Registry.Users]
           |> Seq.map (fun key -> key,key.Name)

    for (subkey,name) in subNodes do
        registryType.AddMemberDelayed (createRegistryNode(subkey,name))

    registryType