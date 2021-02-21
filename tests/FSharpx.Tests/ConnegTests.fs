module FSharpx.Http.Tests.ConnegTests

open FSharpx.Http.Conneg
open NUnit.Framework
open FsUnitTyped

[<Test>]
let ``Parse AcceptLanguage``() =
    let acceptLang = "en-US; q=0.6, de-de;q=0.4,de;q=0.2,en-ca,en;q=0.8, pepe;q=0"
    acceptLang |> parseFilterSortAccept |> shouldEqual [("en-ca", 1.); ("en", 0.8); ("en-us", 0.6); ("de-de", 0.4); ("de", 0.2)]

[<Test>]
let ``Parse Accept``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1"
    accept |> parseFilterSortAccept |> shouldEqual [("text/html;level=2", 0.8); ("text/html;level=1", 0.2)]

[<Test>]
let ``Parse Accept with implicit precedence``() =
    // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1
    let accept = "text/*, text/html, text/html;level=1, */*"
    accept |> parseFilterSortAccept |> List.map fst |> shouldEqual ["text/html;level=1"; "text/html"; "text/*"; "*/*"]

[<Test>]
let ``Media type matching``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1, image/png, image/jpeg;q=0.8"
    let images = accept |> filterMediaTypes "image"
    match images with
    | ("image/jpeg",_)::_ -> failwith "should have been image/png"
    | ("image/png", 1.)::_ -> ()
    | x -> failwithf "can't handle any of these content types: %A, failing with 406" x

[<Test>]
let ``Media type active pattern``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1, image/png, image/jpeg;q=0.8"
    let images = accept |> filterMediaTypes "image" |> List.map fst
    match images with
    | AcceptsMedia "image/png" -> ()
    | AcceptsMedia "image/jpeg" -> ()
    | AcceptsMedia "image/*" -> ()
    | x -> failwithf "can't handle any of these content types: %A, failing with 406" x

[<Test>]
let ``Active pattern with wildcard``() =
    let r = (|AcceptsMedia|_|) "text/plain" ["text/*"]
    r |> shouldEqual (Some())

[<Test>]
let ``matchMediaType match wildcard in server``() =
    matchMediaType "text/*" "text/plain" |> shouldEqual (Some "text/plain")

[<Test>]
let ``matchMediaType match wildcard in client``() =
    matchMediaType "text/plain" "text/*" |> shouldEqual (Some "text/plain")

[<Test>]
let ``matchMediaType match exact``() =
    matchMediaType "text/plain" "text/plain" |> shouldEqual (Some "text/plain") 

[<Test>]
let ``matchMediaType match any in server``() =
    matchMediaType "*/*" "text/plain" |> shouldEqual (Some "text/plain")

[<Test>]
let ``matchMediaType match any in client``() =
    matchMediaType "text/plain" "*/*" |> shouldEqual (Some "text/plain")

[<Test>]
let ``Negotiate media type``() =
    let accept = "application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5" // actual Chrome Accepts header
    let serves = ["application/xml"; "text/xml"; "text/html"; "application/json"]
    let sorted = negotiateMediaType serves accept
    sorted |> shouldEqual ["application/xml",1.; "text/html",0.9; "text/xml",0.5; "application/json",0.5]

[<Test>]
let ``Negotiate media type 2``() =
    let accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" // actual Firefox Accepts header
    let serves = ["text/html"; "application/json"; "application/xml"; "text/xml"]
    let sorted = negotiateMediaType serves accept
    sorted |> shouldEqual ["text/html",1.; "application/xml",0.9; "application/json",0.8; "text/xml",0.8]

[<Test>]
let ``match language with server wildcard``() =
    matchLanguage "*" "en-gb" |> shouldEqual (Some "en-gb")

[<Test>]
let ``match language with client wildcard``() =
    matchLanguage "en-gb" "*" |> shouldEqual (Some "en-gb")

[<Test>]
let ``match language exact``() =
    matchLanguage "en-gb" "en-gb" |> shouldEqual (Some "en-gb")

[<Test>]
let ``no language match``() =
    matchLanguage "en-ca" "en-gb" |> shouldEqual None

[<Test>]
let ``match server more specific than client``() =
     matchLanguage "en-ca" "en" |> shouldEqual (Some "en-ca")

[<Test>]
let ``match server more specific than client2``() =
    matchLanguage "x-pig-latin" "x-pig" |> shouldEqual (Some "x-pig-latin")

[<Test>]
let ``no match client more specific than server``() =
    matchLanguage "en" "en-ca" |> shouldEqual None

[<Test>]
let ``Negotiate languages``() =
    let accept = "da, en-gb;q=0.8, en;q=0.7"
    let serves = ["en-us"]
    let sorted = negotiateLanguage serves accept
    sorted |> shouldEqual ["en-us",0.7]

[<Test>]
let ``Negotiate charset with implicit iso-8859-1``() =
    let accept = "iso-8859-5, unicode-1-1;q=0.8"
    let serves = ["iso-8859-1"]
    let sorted = negotiateCharset serves accept
    sorted |> shouldEqual ["iso-8859-1",1.]

[<Test>]
let ``Negotiate charset with implicit iso-8859-1 in wildcard with q``() =
    let accept = "iso-8859-5, unicode-1-1;q=0.8, *;q=0.7"
    let serves = ["iso-8859-1"; "unicode-1-1"]
    let sorted = negotiateCharset serves accept
    sorted |> shouldEqual ["unicode-1-1",0.8; "iso-8859-1",0.7]

[<Test>]
let ``Negotiate charset with implicit iso-8859-1 in wildcard with q=0``() =
    let accept = "iso-8859-5, unicode-1-1;q=0.8, *;q=0"
    let serves = ["iso-8859-1"; "unicode-1-1"]
    let sorted = negotiateCharset serves accept
    sorted |> shouldEqual ["unicode-1-1",0.8]

[<Test>]
let ``Negotiate charset with explicit iso-8859-1 and wildcard with q``() =
    let accept = "iso-8859-5, iso-8859-1;q=0.9, unicode-1-1;q=0.8, *;q=0.7"
    let serves = ["iso-8859-1"; "unicode-1-1"]
    let sorted = negotiateCharset serves accept
    sorted |> shouldEqual ["iso-8859-1",0.9; "unicode-1-1",0.8]

[<Test>]
let ``Negotiate charset with empty accept``() =
    let accept: string = null
    let serves = ["iso-8859-1"; "unicode-1-1"]
    let sorted = negotiateCharset serves accept
    sorted |> shouldEqual ["iso-8859-1",1.; "unicode-1-1",1.]

[<Test>]
let ``match encoding empty``() =
    let accept: string = null
    let serves = ["compress"; "gzip"]
    let neg = negotiateEncoding serves accept
    neg |> shouldEqual ["compress",1.; "gzip",1.]

[<Test>]
let ``match encoding implicit identity``() =
    let accept: string = "gzip, compress;q=0.8"
    let serves = ["compress"; "identity"]
    let neg = negotiateEncoding serves accept
    neg |> shouldEqual ["compress",0.8; "identity",0.001]

[<Test>]
let ``match encoding identity 0``() =
    let accept: string = "gzip, compress;q=0.8, identity;q=0"
    let serves = ["compress"; "identity"]
    let neg = negotiateEncoding serves accept
    neg |> shouldEqual ["compress",0.8]