module FSharpx.TypeProviders.NamespaceProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Text.RegularExpressions

[<TypeProvider>]
type public SystemProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        DSL.rootNamespace, 
        [FilesTypeProvider.createTypedFileSystem()
         RegistryProvider.typedRegistry])

[<TypeProviderAssembly>]
do ()