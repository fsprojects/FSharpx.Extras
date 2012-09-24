module FSharpx.TypeProviders.NamespaceProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Text.RegularExpressions

[<TypeProvider>]
type public MathProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(DSL.rootNamespace,[VectorTypeProvider.vectorTypeProvider])

[<TypeProviderAssembly>]
do ()