namespace FSharpx.Core.Tests

open System
open System.Threading
open System.Threading.Tasks

open FSharpx

open FsUnit
open NUnit.Framework

[<TestFixture>]
type ``Option module Extension Tests``() = 

    [<Test>]
    member test.``option d ToString None yields the default value d``() =
         let expected = "Default"
         let result = Option.option expected (fun i -> i.ToString()) None
         result |> should equal expected
         
    [<Test>]
    member test.``option d ToString (Some 42) yields Some "42")``() =
         let expected = "42"
         let result = Option.option "0" (fun i -> i.ToString()) (Some 42)
         result |> should equal expected

    [<Test>]
    member test.``fromTryPattern using System.Double.TryParse on input "42" is Some 42.0``() =
        let f = Option.fromTryPattern Double.TryParse
        f "42" |> should equal (Some 42.0)
        
    [<Test>]
    member test.``fromTryPattern using System.Double.TryParse on input "xx" is None``() =
        let f = Option.fromTryPattern Double.TryParse
        f "xx" |> should equal None