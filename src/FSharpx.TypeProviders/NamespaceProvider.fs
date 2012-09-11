module FSharpx.TypeProviders.NamespaceProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Text.RegularExpressions

[<TypeProvider>]
type public FSharpxProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        Settings.rootNamespace, 
        [RegexTypeProvider.regexTy
         MiniCsvProvider.csvType this cfg
         FilesTypeProvider.typedFileSystem
         XmlTypeProvider.xmlType this cfg
         StateMachineProvider.stateMachineTy this true cfg
         StateMachineProvider.stateMachineTy this false cfg
         StateMachineProvider.graph this cfg
         JsonTypeProvider.jsonType this cfg
         RegistryProvider.typedRegistry
         XamlProvider.xamlType this cfg
         AppSettingsTypeProvider.typedAppSettings cfg
         ExcelProvider.typExcel cfg
         VectorTypeProvider.vectorTypeProvider])

[<TypeProviderAssembly>]
do ()