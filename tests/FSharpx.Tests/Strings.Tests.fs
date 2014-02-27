module FSharpx.Tests.StringsTest

open System
open FSharpx.Strings
open NUnit.Framework
open FsUnit

[<Test>]
let ``Can pluralize names``() =
   let check a b = pluralize a |> should equal b
   check "Author" "Authors"
   check "Authors" "Authors"
   check "Item" "Items"
   check "Items" "Items"
   check "Entity" "Entities"
   check "goose" "geese"
   check "deer" "deer"
   check "sheep" "sheep"
   check "wolf" "wolves"
   check "volcano" "volcanoes"
   check "aircraft" "aircraft"
   check "alumna" "alumnae"
   check "alumnus" "alumni"
   check "house" "houses"
   check "fungus" "fungi"
   check "woman" "women"
   check "index" "indices"

[<Test>]
let ``Can singularize names``() =
   let check a b = singularize a |> should equal b
   check "Author" "Author"
   check "Authors" "Author"
   check "Item" "Item"
   check "Items" "Item"
   check "Entities" "Entity"
   check "geese" "goose" 
   check "deer" "deer"
   check "sheep" "sheep"
   check "wolves" "wolf"
   check "volcanoes" "volcano"
   check "aircraft" "aircraft"
   check "alumnae" "alumna"
   check "alumni" "alumnus"
   check "houses" "house"
   check "fungi" "fungus"
   check "funguses" "fungus"
   check "women" "woman"
   check "indices" "index"
   check "indexes" "index"

[<Test>]
let ``Can simplify the type names``() = 
    let (=!=) a b = a |> should equal b

    niceName "" =!= "" 
    niceName "__hello__" =!= "Hello"
    niceName "abc" =!= "Abc"
    niceName "hello_world" =!= "HelloWorld"
    niceName "HelloWorld" =!= "HelloWorld"
    niceName "helloWorld" =!= "HelloWorld"
    niceName "hello123" =!= "Hello123"
    niceName "Hello123" =!= "Hello123"
    niceName "hello!123" =!= "Hello123"
    niceName "HelloWorld123_hello__@__omg" =!= "HelloWorld123HelloOmg"
    niceName "HKEY_CURRENT_USER" =!= "HKEY_CURRENT_USER"

[<Test>]
let ``Can infer floats``() = 
    isFloat "42.42" |> should equal true


[<Test>]
let ``Should split by newlines`` () = 
    let a = @"foo biz
bar
baz

"   
    let expected = ["foo biz";"bar";"baz"]

    (a |> toLines)  |> should equal expected

[<Test>]
let ``Should merge by newlines`` () = 
    let a = ["foo biz";"bar";"baz"]
    let expected = @"foo biz
bar
baz"   

    (a |> joinLines) |> should equal expected

[<Test>]
let ``Should merge by whitespace`` () = 
    let a = ["foo";"bar";"baz"]
    let expected = "foo bar baz"   

    (a |> joinWords) |> should equal expected

[<Test>]
let ``Should split by whitespace`` () = 
    let a = @"foo bar baz"      

    let expected = ["foo";"bar";"baz"]

    (a |> toWords) |> should equal expected

[<Test>]
let ``Should split by whitespace with tabs`` () = 
    let a = @"foo bar       baz"      

    let expected = ["foo";"bar";"baz"]

    (a |> toWords) |> should equal expected

[<Test>]
let ``Should split by whitespace with mixed spaces and tabs`` () = 
    let a = @"          foo bar       baz   "      

    let expected = ["foo";"bar";"baz"]

    (a |> toWords) |> should equal expected

[<Test>]
let ``Should split by whitespace with \n newlines`` () = 
    let a = "  foo bar    \n   \nbaz"      

    let expected = ["foo";"bar";"baz"]

    (a |> toWords) |> should equal expected

[<Test>]
let ``Should split by whitespace with \r newlines`` () = 
    let a = " foo bar    \r   \rbaz"      

    let expected = ["foo";"bar";"baz"]

    (a |> toWords) |> should equal expected

[<Test>]
let ``Should split by whitespace with \r\n newlines`` () = 
    let a = "           foo bar    \r\n   \r\nbaz"      

    let expected = ["foo";"bar";"baz"]

    (a |> toWords) |> should equal expected

[<Test>]
let ``Should split by whitespace with mixed combinations of \r and \n newlines`` () = 
    let a = "foo bar    \n\r\n   \r\r\r\nbaz"      

    let expected = ["foo";"bar";"baz"]

    (a |> toWords) |> should equal expected