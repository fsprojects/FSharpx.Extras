module FSharpx.TypeProviders.Tests.RegexTests

open NUnit.Framework
open FSharpx
open FsUnit

type PhoneRegex = Regex< @"(?<AreaCode>^\d{3})-(?<PhoneNumber>\d{3}-\d{4}$)">

[<Test>] 
let ``Can call typed IsMatch function``() =      
    PhoneRegex.IsMatch "425-123-2345"
    |> should equal true

[<Test>] 
let ``Can call typed CompleteMatch function``() =      
    PhoneRegex().Match("425-123-2345").CompleteMatch.Value
    |> should equal "425-123-2345"

[<Test>] 
let ``Can return AreaCode in simple phone number``() =
    PhoneRegex().Match("425-123-2345").AreaCode.Value
    |> should equal "425"

[<Test>] 
let ``Can return PhoneNumber property in simple phone number``() =
    PhoneRegex().Match("425-123-2345").PhoneNumber.Value
    |> should equal "123-2345"