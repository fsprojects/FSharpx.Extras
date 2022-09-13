module FSharpx.Tests.StringsTest

open System
open FSharpx.Text.Strings
open NUnit.Framework
open FsUnitTyped

let pluralizeTestCases =
    [
        "Author",   "Authors"
        "Authors",  "Authors"
        "Item",     "Items"
        "Items",    "Items"
        "Entity",   "Entities"
        "goose",    "geese"
        "deer",     "deer"
        "sheep",    "sheep"
        "wolf",     "wolves"
        "volcano",  "volcanoes"
        "aircraft", "aircraft"
        "alumna",   "alumnae"
        "alumnus",  "alumni"
        "house",    "houses"
        "fungus",   "fungi"
        "woman",    "women"
        "index",    "indices"
    ] |> List.map TestCaseData

[<Test>]
[<TestCaseSource(nameof(pluralizeTestCases))>]
let ``Can pluralize names`` a b =
    pluralize a |> shouldEqual b

let singularizeTestCases =
    [
        "Author",    "Author"
        "Authors",   "Author"
        "Item",      "Item"
        "Items",     "Item"
        "Entities",  "Entity"
        "geese",     "goose" 
        "deer",      "deer"
        "sheep",     "sheep"
        "wolves",    "wolf"
        "volcanoes", "volcano"
        "aircraft",  "aircraft"
        "alumnae",   "alumna"
        "alumni",    "alumnus"
        "houses",    "house"
        "fungi",     "fungus"
        "funguses",  "fungus"
        "women",     "woman"
        "indices",   "index"
        "indexes",   "index"
    ] |> List.map TestCaseData

[<Test>]
[<TestCaseSource(nameof(singularizeTestCases))>]
let ``Can singularize names`` a b =
    singularize a |> shouldEqual b

let simplifyTypeTestCases =
    [
        "",                            "" 
        "__hello__",                   "Hello"
        "abc",                         "Abc"
        "hello_world",                 "HelloWorld"
        "HelloWorld",                  "HelloWorld"
        "helloWorld",                  "HelloWorld"
        "hello123",                    "Hello123"
        "Hello123",                    "Hello123"
        "hello!123",                   "Hello123"
        "HelloWorld123_hello__@__omg", "HelloWorld123HelloOmg"
        "HKEY_CURRENT_USER",           "HKEY_CURRENT_USER"
    ] |> List.map TestCaseData

[<Test>]
[<TestCaseSource(nameof(simplifyTypeTestCases))>]
let ``Can simplify the type names`` a b =
    niceName a |> shouldEqual b

[<Test>]
let ``Can infer floats``() = 
    isFloat "42.42" |> shouldEqual true


[<Test>]
let ``Should split by newlines`` () = 
    let a = @"foo biz
bar
baz

"   
    let expected = ["foo biz";"bar";"baz"] :> seq<_>

    a |> toLines |> shouldEqual expected

[<Test>]
let ``Should merge by newlines`` () = 
    let a = ["foo biz";"bar";"baz"]
    let osSpecificNewLine = System.Environment.NewLine
    let expected = sprintf "foo biz%sbar%sbaz" osSpecificNewLine osSpecificNewLine

    a |> joinLines |> shouldEqual expected

[<Test>]
let ``Should merge by whitespace`` () = 
    let a = ["foo";"bar";"baz"]
    let expected = "foo bar baz"   

    a |> joinWords |> shouldEqual expected

[<Test>]
let ``Should split by whitespace`` () = 
    let a = @"foo bar baz"      

    let expected = ["foo";"bar";"baz"] :> seq<_>

    a |> toWords |> shouldEqual expected

[<Test>]
let ``Should split by whitespace with tabs`` () = 
    let a = @"foo bar       baz"      

    let expected = ["foo";"bar";"baz"] :> seq<_>

    a |> toWords |> shouldEqual expected

[<Test>]
let ``Should split by whitespace with mixed spaces and tabs`` () = 
    let a = @"          foo bar       baz   "      

    let expected = ["foo";"bar";"baz"] :> seq<_>

    a |> toWords |> shouldEqual expected

[<Test>]
let ``Should split by whitespace with \n newlines`` () = 
    let a = "  foo bar    \n   \nbaz"      

    let expected = ["foo";"bar";"baz"] :> seq<_>

    a |> toWords |> shouldEqual expected

[<Test>]
let ``Should split by whitespace with \r newlines`` () = 
    let a = " foo bar    \r   \rbaz"      

    let expected = ["foo";"bar";"baz"] :> seq<_>

    a |> toWords |> shouldEqual expected

[<Test>]
let ``Should split by whitespace with \r\n newlines`` () = 
    let a = "           foo bar    \r\n   \r\nbaz"      

    let expected = ["foo";"bar";"baz"] :> seq<_>


    a |> toWords |> shouldEqual expected

[<Test>]
let ``Should split by whitespace with mixed combinations of \r and \n newlines`` () = 
    let a = "foo bar    \n\r\n   \r\r\r\nbaz"      

    let expected = ["foo";"bar";"baz"] :> seq<_>

    a |> toWords |> shouldEqual expected