module FSharpx.TypeProviders.NamespaceProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Text.RegularExpressions

[<TypeProvider>]
type public FSharpxProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        DSL.rootNamespace, 
        [RegexTypeProvider.regexTy         
         FilesTypeProvider.typedFileSystem
         RegistryProvider.typedRegistry
         XamlProvider.xamlType this cfg
         AppSettingsTypeProvider.typedAppSettings cfg
         ExcelProvider.typExcel cfg
         VectorTypeProvider.vectorTypeProvider])

[<TypeProviderAssembly>]
do ()