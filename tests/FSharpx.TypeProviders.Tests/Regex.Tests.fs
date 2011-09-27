module FSharp.TypeProviders.Tests.RegexTests

open NUnit.Framework
open FSharpx
open FsUnit

type T = RegexTyped< @"(?<AreaCode>^\d{3})-(?<PhoneNumber>\d{3}-\d{4}$)">
let reg = T()

[<Test>] 
let ``Can call typed IsMatch function``() =      
    Assert.IsTrue <| T.IsMatch "425-123-2345"

[<Test>] 
let ``Can call typed CompleteMatch function``() =      
    reg.Match("425-123-2345").CompleteMatch.Value
    |> should equal "425-123-2345"

[<Test>] 
let ``Can parse simple phone number``() =
    reg.Match("425-123-2345").AreaCode.Value
    |> should equal "425"