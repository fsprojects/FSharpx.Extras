module FSharpx.TypeProviders.NamespaceProvider

open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes

[<TypeProvider>]
type public DocumentProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        DSL.rootNamespace, 
        [XmlTypeProvider.xmlType this cfg
         JsonTypeProvider.jsonType this cfg
         MiniCsvProvider.csvType this cfg])

[<TypeProviderAssembly>]
do ()