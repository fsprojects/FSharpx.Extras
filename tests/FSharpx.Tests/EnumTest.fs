module FSharpx.Tests.EnumTests
open NUnit.Framework
open FsUnitTyped
open FSharpx
open FsUnitTyped

type LanguageOptions=
    | FSharp = 0
    | CSharp = 1
    | VB = 2
    
[<Test>] 
let ``tryParse can parse value and return Some value``() =
    "CSharp" |> Enum.tryParse |> shouldEqual (Some LanguageOptions.CSharp) 

[<Test>] 
let ``tryParse returns None if it fails to parse``() =
    "English" |> Enum.tryParse<LanguageOptions> |> shouldEqual None

[<Test>] 
let ``parse returns parsed value``() =
    "CSharp" |> Enum.parse |> shouldEqual LanguageOptions.CSharp 

[<Test>] 
let ``parse throws an exception when it fails to parse`` ()=
    let parseEnglish ()=
        let x = Enum.parse("English") : LanguageOptions
        ()
    parseEnglish |> shouldFail

let isDefinedTestCases =
    [
        LanguageOptions.CSharp
        LanguageOptions.FSharp
        LanguageOptions.VB
    ]
[<TestCaseSource("isDefinedTestCases")>]
let ``isDefined returns true when valid simple enum value is checked`` (input:LanguageOptions) =
    input |> Enum.isDefined |> shouldEqual true

[<Test>] 
let ``isDefined returns false when not valid simple enum value is checked``() =
    let invalidEnum : LanguageOptions = enum 3  
    invalidEnum |> Enum.isDefined |> shouldEqual false

[<System.Flags>]
type FlaggedLanguageOptions =
    | FSharp = 0
    | CSharp = 1
    | VB = 2

[<Test>] 
let ``isDefined returns true when valid flagged enum value is checked``() =
    let validFlagEnum = FlaggedLanguageOptions.CSharp ||| FlaggedLanguageOptions.FSharp ||| FlaggedLanguageOptions.VB
    validFlagEnum |> Enum.isDefined |> shouldEqual true

[<Test>] 
let ``isDefined returns false when not valid flagged enum value is checked`` ()=
    let invalidEnum : FlaggedLanguageOptions = enum 300
    invalidEnum |> Enum.isDefined |> shouldEqual false

[<Test>]
let ``toString should handle FSharp enums`` () =
    let result = Enum.toString LanguageOptions.FSharp
    result |> shouldEqual "FSharp"

open System.Globalization
[<Test>]
let ``toString should  handle dotnet enums`` () =
    let result = Enum.toString NumberStyles.Float
    result |> shouldEqual "Float"

[<Test>] 
let ``getValues works as expected for dotnet enum`` ()=
    let result = Enum.getValues<System.Globalization.NumberStyles>()
    let expectedResult =
        [NumberStyles.None; NumberStyles.AllowLeadingWhite; NumberStyles.AllowTrailingWhite; NumberStyles.AllowLeadingSign; NumberStyles.Integer;
         NumberStyles.AllowTrailingSign; NumberStyles.AllowParentheses; NumberStyles.AllowDecimalPoint; NumberStyles.AllowThousands; NumberStyles.Number;
         NumberStyles.AllowExponent; NumberStyles.Float; NumberStyles.AllowCurrencySymbol; NumberStyles.Currency; NumberStyles.Any; NumberStyles.AllowHexSpecifier;
         NumberStyles.HexNumber]
    result |> Seq.toList |> shouldEqual expectedResult
