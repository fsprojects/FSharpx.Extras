module FSharpx.TypeProviders.NamespaceProvider

open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Text.RegularExpressions

[<TypeProvider>]
type public FSharpxProvider() as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(
        Settings.rootNamespace, 
        [RegexTypeProvider.regexTy
         FilesTypeProvider.fileTy])

[<TypeProviderAssembly>]
do ()