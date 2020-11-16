module FSharpx.Tests.EnumTests
open NUnit.Framework
open FsUnitTyped
open FSharpx

type LanguageOptions=
    | FSharp = 0
    | CSharp = 1
    | VB = 2
    
[<Test>] 
let ``tryParse can parse value and return Some value`` ()=
    Assert.AreEqual(Some LanguageOptions.CSharp, (Enum.tryParse("CSharp") : LanguageOptions option))

[<Test>] 
let ``tryParse returns None if it fails to parse`` ()=
    Assert.AreEqual(None, (Enum.tryParse("English") : LanguageOptions option))

[<Test>] 
let ``parse returns parsed value`` ()=
    Assert.AreEqual(LanguageOptions.CSharp ,(Enum.parse("CSharp") : LanguageOptions))

[<Test>] 
let ``parse throws an exception when it fails to parse`` ()=
    let parseEnglish ()=
        let x = Enum.parse("English") : LanguageOptions
        ()
    parseEnglish |> shouldFail

[<Test>] 
let ``getValues works as expected for F# enum`` ()=
    let result = Enum.getValues<LanguageOptions>()
    result |> Seq.toList |> shouldEqual [LanguageOptions.FSharp; LanguageOptions.CSharp; LanguageOptions.VB]

[<Test>] 
let ``isDefined returns true when valid simple enum value is checked`` ()=
    Assert.IsTrue(Enum.isDefined LanguageOptions.CSharp && Enum.isDefined LanguageOptions.FSharp && Enum.isDefined LanguageOptions.VB)

[<Test>] 
let ``isDefined returns false when not valid simple enum value is checked`` ()=
    let invalidEnum : LanguageOptions = enum 3
    Assert.IsFalse(Enum.isDefined invalidEnum)

[<System.Flags>]
type FlaggedLanguageOptions =
    | FSharp = 0
    | CSharp = 1
    | VB = 2

[<Test>] 
let ``isDefined returns true when valid flagged enum value is checked`` ()=
    Assert.IsTrue(Enum.isDefined (FlaggedLanguageOptions.CSharp ||| FlaggedLanguageOptions.FSharp ||| FlaggedLanguageOptions.VB))

[<Test>] 
let ``isDefined returns false when not valid flagged enum value is checked`` ()=
    let invalidEnum : FlaggedLanguageOptions = enum 300
    Assert.IsFalse(Enum.isDefined invalidEnum)

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
