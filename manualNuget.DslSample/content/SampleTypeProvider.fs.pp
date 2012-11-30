module $rootnamespace$

open FSharpx.TypeProviders.DSL
open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharpPreviewRelease2011.ProvidedTypes
open System.Reflection

let rootNamespace = "$rootnamespace$"
let thisAssembly = Assembly.GetExecutingAssembly()

let addMyTypedMembers (param:string) (tyDef:ProvidedTypeDefinition) = 
    try        
        tyDef 
        |+> literalField "MySampleFieldName" param
        |> addXmlDoc (sprintf "Returns the string that was provided as the static parameter (i.e. %s)" param)
        |> ignore

        tyDef
    with 
    | exn -> tyDef
    
let typedMySample () =
    erasedType<obj> thisAssembly rootNamespace "MySample"
      |> staticParameter "sampleParam" (fun typeName param -> 
            erasedType<obj> thisAssembly rootNamespace typeName
                |> addMyTypedMembers param)

[<TypeProvider>]
type public SampleTypeProvider() as this =
    inherit TypeProviderForNamespaces()

    do this.AddNamespace(rootNamespace, [typedMySample()])

[<TypeProviderAssembly>]
do ()