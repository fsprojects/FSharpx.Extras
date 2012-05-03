module FSharpx.TypeProviders.Tests.RegistryTests

open FSharpx
open NUnit.Framework
open FsUnit

[<Test>] 
let ``Can create type for HKEY_CURRENT_USER``() = 
    Registry.HKEY_CURRENT_USER.Path 
      |> should equal "HKEY_CURRENT_USER"

[<Test>] 
let ``Can create subtype for HKEY_LOCAL_MACHINE``() = 
    Registry.HKEY_LOCAL_MACHINE.SOFTWARE.Path 
      |> should equal @"HKEY_LOCAL_MACHINE\SOFTWARE"