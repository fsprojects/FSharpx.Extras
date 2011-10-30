module Tests

open Xunit
open FsConneg

[<Fact>]
let ``Parse AcceptLanguage``() =
    let acceptLang = "en-US; q=0.6, de-de;q=0.4,de;q=0.2,en-ca,en;q=0.8, pepe;q=0"
    match parseFilterSortAccept acceptLang with
    | ["en-ca",1.; "en",0.8; "en-us",0.6; "de-de",0.4; "de",0.2] -> ()
    | x -> failwithf "wrong parsing: %A" x

[<Fact>]
let ``Parse Accept``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1"
    match parseFilterSortAccept accept with
    | ["text/html;level=2",0.8; "text/html;level=1",0.2] -> () // 0.001 added for having an additional parameter
    | x -> failwithf "wrong parsing: %A" x

[<Fact>]
let ``Parse Accept with implicit precedence``() =
    // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1
    let accept = "text/*, text/html, text/html;level=1, */*"
    match parseFilterSortAccept accept with
    | ("text/html;level=1",_)::("text/html",_)::("text/*",_)::("*/*",_)::[] -> ()
    | x -> failwithf "wrong parsing: %A" x

[<Fact>]
let ``Media type matching``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1, image/png, image/jpeg;q=0.8"
    let images = accept |> filterMediaTypes "image"
    match images with
    | ("image/jpeg",_)::_ -> failwith "should have been image/png"
    | ("image/png", 1.)::_ -> ()
    | x -> failwithf "can't handle any of these content types: %A, failing with 406" x

[<Fact>]
let ``Media type active pattern``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1, image/png, image/jpeg;q=0.8"
    let images = accept |> filterMediaTypes "image" |> List.map fst
    match images with
    | AcceptsMedia "image/png" -> ()
    | AcceptsMedia "image/jpeg" -> ()
    | AcceptsMedia "image/*" -> ()
    | x -> failwithf "can't handle any of these content types: %A, failing with 406" x

[<Fact>]
let ``Active pattern with wildcard``() =
    let r = (|AcceptsMedia|_|) "text/plain" ["text/*"]
    Assert.Equal(Some (), r)

[<Fact>]
let ``matchMediaType match wildcard in server``() =
    Assert.Equal(Some "text/plain", matchMediaType "text/*" "text/plain")

[<Fact>]
let ``matchMediaType match wildcard in client``() =
    Assert.Equal(Some "text/plain", matchMediaType "text/plain" "text/*")

[<Fact>]
let ``matchMediaType match exact``() =
    Assert.Equal(Some "text/plain", matchMediaType "text/plain" "text/plain")

[<Fact>]
let ``matchMediaType match any in server``() =
    Assert.Equal(Some "text/plain", matchMediaType "*/*" "text/plain")

[<Fact>]
let ``matchMediaType match any in client``() =
    Assert.Equal(Some "text/plain", matchMediaType "text/plain" "*/*")

[<Fact>]
let ``Negotiate media type``() =
    let accept = "application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5" // actual Chrome Accepts header
    let serves = ["application/xml"; "text/xml"; "text/html"; "application/json"]
    let sorted = negotiateMediaType serves accept
    Assert.Equal(["application/xml",1.; "text/html",0.9; "text/xml",0.5; "application/json",0.5], sorted)

[<Fact>]
let ``Negotiate media type 2``() =
    let accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" // actual Firefox Accepts header
    let serves = ["text/html"; "application/json"; "application/xml"; "text/xml"]
    let sorted = negotiateMediaType serves accept
    Assert.Equal(["text/html",1.; "application/xml",0.9; "application/json",0.8; "text/xml",0.8], sorted)

[<Fact>]
let ``match language with server wildcard``() =
    Assert.Equal(Some "en-gb", matchLanguage "*" "en-gb")

[<Fact>]
let ``match language with client wildcard``() =
    Assert.Equal(Some "en-gb", matchLanguage "en-gb" "*")

[<Fact>]
let ``match language exact``() =
    Assert.Equal(Some "en-gb", matchLanguage "en-gb" "en-gb")

[<Fact>]
let ``no language match``() =
    Assert.Equal(None, matchLanguage "en-ca" "en-gb")

[<Fact>]
let ``match server more specific than client``() =
    Assert.Equal(Some "en-ca", matchLanguage "en-ca" "en")

[<Fact>]
let ``match server more specific than client2``() =
    Assert.Equal(Some "x-pig-latin", matchLanguage "x-pig-latin" "x-pig")

[<Fact>]
let ``no match client more specific than server``() =
    Assert.Equal(None, matchLanguage "en" "en-ca")

[<Fact>]
let ``Negotiate languages``() =
    let accept = "da, en-gb;q=0.8, en;q=0.7"
    let serves = ["en-us"]
    let sorted = negotiateLanguage serves accept
    Assert.Equal(["en-us",0.7], sorted)

[<Fact>]
let ``Negotiate charset with implicit iso-8859-1``() =
    let accept = "iso-8859-5, unicode-1-1;q=0.8"
    let serves = ["iso-8859-1"]
    let sorted = negotiateCharset serves accept
    Assert.Equal(["iso-8859-1",1.], sorted)

[<Fact>]
let ``Negotiate charset with implicit iso-8859-1 in wildcard with q``() =
    let accept = "iso-8859-5, unicode-1-1;q=0.8, *;q=0.7"
    let serves = ["iso-8859-1"; "unicode-1-1"]
    let sorted = negotiateCharset serves accept
    Assert.Equal(["unicode-1-1",0.8; "iso-8859-1",0.7], sorted)

[<Fact>]
let ``Negotiate charset with implicit iso-8859-1 in wildcard with q=0``() =
    let accept = "iso-8859-5, unicode-1-1;q=0.8, *;q=0"
    let serves = ["iso-8859-1"; "unicode-1-1"]
    let sorted = negotiateCharset serves accept
    Assert.Equal(["unicode-1-1",0.8], sorted)

[<Fact>]
let ``Negotiate charset with explicit iso-8859-1 and wildcard with q``() =
    let accept = "iso-8859-5, iso-8859-1;q=0.9, unicode-1-1;q=0.8, *;q=0.7"
    let serves = ["iso-8859-1"; "unicode-1-1"]
    let sorted = negotiateCharset serves accept
    Assert.Equal(["iso-8859-1",0.9; "unicode-1-1",0.8], sorted)

[<Fact>]
let ``Negotiate charset with empty accept``() =
    let accept: string = null
    let serves = ["iso-8859-1"; "unicode-1-1"]
    let sorted = negotiateCharset serves accept
    Assert.Equal(["iso-8859-1",1.; "unicode-1-1",1.], sorted)

[<Fact>]
let ``match encoding empty``() =
    let accept: string = null
    let serves = ["compress"; "gzip"]
    let neg = negotiateEncoding serves accept
    Assert.Equal(["compress",1.; "gzip",1.], neg)

[<Fact>]
let ``match encoding implicit identity``() =
    let accept: string = "gzip, compress;q=0.8"
    let serves = ["compress"; "identity"]
    let neg = negotiateEncoding serves accept
    Assert.Equal(["compress",0.8; "identity",0.001], neg)

[<Fact>]
let ``match encoding identity 0``() =
    let accept: string = "gzip, compress;q=0.8, identity;q=0"
    let serves = ["compress"; "identity"]
    let neg = negotiateEncoding serves accept
    Assert.Equal(["compress",0.8], neg)