[<NUnit.Framework.SetUpFixture>]
module FSharpx.Tests.TestSetup
open NUnit.Framework
open System.IO
open System.Reflection

[<OneTimeSetUp>]
let ``Test working directory workaround for net462`` () =
    Assembly.GetExecutingAssembly().Location
    |> Path.GetDirectoryName
    |> Directory.SetCurrentDirectory

