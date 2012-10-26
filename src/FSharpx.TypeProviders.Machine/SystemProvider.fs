module FSharpx.TypeProviders.NamespaceProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Text.RegularExpressions
open FSharpx.TypeProviders.Helper

[<TypeProvider>]
type public SystemProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        rootNamespace, 
        [FilesTypeProvider.createTypedFileSystem()
         RegistryProvider.createTypedRegistry()])

[<TypeProviderAssembly>]
do ()