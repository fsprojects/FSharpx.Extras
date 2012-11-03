module FSharpx.TypeProviders.NamespaceProvider

open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Helper

[<TypeProvider>]
type public ProtocolBuffersProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        rootNamespace, 
        [ProtoBufProvider.createProtoBufType this cfg])

[<TypeProviderAssembly>]
do ()