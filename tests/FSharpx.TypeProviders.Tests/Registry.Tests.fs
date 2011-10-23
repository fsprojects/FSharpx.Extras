module FSharp.TypeProviders.Tests.RegistryTests

open FSharpx
open NUnit.Framework
open FsUnit

type LocalRegistry = RegistryTyped<""> // Todo: Try to get rid of this useless static parameter

[<Test>] 
let ``Can create type for HKEY_CURRENT_USER``() = 
    LocalRegistry.HKEY_CURRENT_USER.Path 
      |> should equal @"HKEY_CURRENT_USER"

[<Test>] 
let ``Can create subtype for HKEY_LOCAL_MACHINE``() = 
    LocalRegistry.HKEY_LOCAL_MACHINE.SOFTWARE.Path 
      |> should equal @"HKEY_LOCAL_MACHINE\SOFTWARE"