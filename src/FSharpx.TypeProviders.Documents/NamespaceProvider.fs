module FSharpx.TypeProviders.NamespaceProvider

open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open FSharpx.TypeProviders.Helper

[<TypeProvider>]
type public DocumentProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        rootNamespace, 
        [XmlTypeProvider.xmlType this cfg
         JsonTypeProvider.jsonType this cfg
         MiniCsvProvider.csvType this cfg])

[<TypeProviderAssembly>]
do ()