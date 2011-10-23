module FSharp.TypeProviders.Tests.RegistryTests

open FSharpx
open NUnit.Framework
open FsUnit

[<Test>] 
let ``Can create type for HKEY_CURRENT_USER``() = 
    RegistryTyped.HKEY_CURRENT_USER.Path 
      |> should equal @"HKEY_CURRENT_USER"

[<Test>] 
let ``Can create subtype for HKEY_LOCAL_MACHINE``() = 
    RegistryTyped.HKEY_LOCAL_MACHINE.SOFTWARE.Path 
      |> should equal @"HKEY_LOCAL_MACHINE\SOFTWARE"

[<Test>] 
let ``Can open flash property of HKEY_LOCAL_MACHINE``() = 
    RegistryTyped.HKEY_LOCAL_MACHINE.SOFTWARE.flash
      |> should equal @"application/x-shockwave-flash"