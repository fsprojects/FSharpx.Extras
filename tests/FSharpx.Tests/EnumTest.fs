module FSharpx.Http.Tests.EnumTests
open NUnit.Framework
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

let ``parse throws an exception when it fails to parse`` ()=
    let parseEnglish ()=
        let x = Enum.parse("English") : LanguageOptions
        ()
    Assert.Throws<System.Exception>( TestDelegate( parseEnglish ) )  