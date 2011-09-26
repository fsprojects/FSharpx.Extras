module FSharp.TypeProviders.Tests.RegexTests

open NUnit.Framework
open FSharpx
open FsUnit

type T = RegexTyped< @"(?<AreaCode>^\d{3})-(?<PhoneNumber>\d{3}-\d{4}$)">

[<Test>] 
let ``Can parse simple phone number``() = 
    
    let reg = T() 
    let result = T.IsMatch("425-123-2345")
    reg.Match("425-123-2345").AreaCode.Value
    |> should equal "425"