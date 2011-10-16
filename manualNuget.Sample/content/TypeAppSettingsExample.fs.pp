module $rootnamespace$

open FSharpx

type settings = AppSettingsTyped< "Test.config" >

printfn "TestInt1 Value is %i" settings.TestInt1
