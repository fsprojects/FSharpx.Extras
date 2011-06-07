module Tests

open Xunit
open FsConneg

[<Fact>]
let ``Parse AcceptLanguage``() =
    let acceptLang = "en-US; q=0.6, de-de;q=0.4,de;q=0.2,en-ca,en;q=0.8, pepe;q=0"
    match parseAccept acceptLang with
    | ["en-ca"; "en"; "en-us"; "de-de"; "de"] -> ()
    | x -> failwithf "wrong parsing: %A" x

[<Fact>]
let ``Parse Accept``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1"
    match parseAccept accept with
    | ["text/html;level=2"; "text/html;level=1"] -> ()
    | x -> failwithf "wrong parsing: %A" x

[<Fact>]
let ``Parse Accept with implicit precedence``() =
    // http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1
    let accept = "text/*, text/html, text/html;level=1, */*"
    match parseAccept accept with
    | ["text/html;level=1"; "text/html"; "text/*"; "*/*"] -> ()
    | x -> failwithf "wrong parsing: %A" x

[<Fact>]
let ``Best content type``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1, text/plain, image/jpeg"
    match bestMediaType "text" accept with
    | None -> failwith "no suitable content type found"
    | Some contentType -> Assert.Equal("text/plain", contentType)

[<Fact>]
let ``Best content type, none found``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1, text/plain, image/jpeg"
    Assert.Equal(None, bestMediaType "application" accept)

[<Fact>]
let ``Content type matching``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1, image/png, image/jpeg;q=0.8"
    let images = accept |> filterMediaTypes "image"
    match images with
    | "image/jpeg"::_ -> failwith "should have been image/png"
    | "image/png"::_ -> ()
    | x -> failwithf "can't handle any of these content types: %A, failing with 406" x

[<Fact>]
let ``Content type active pattern``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1, image/png, image/jpeg;q=0.8"
    let images = accept |> filterMediaTypes "image"
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
let ``matchMedia match wildcard in server``() =
    Assert.Equal(Some "text/plain", matchMedia "text/*" "text/plain")

[<Fact>]
let ``matchMedia match wildcard in client``() =
    Assert.Equal(Some "text/plain", matchMedia "text/plain" "text/*")

[<Fact>]
let ``matchMedia match exact``() =
    Assert.Equal(Some "text/plain", matchMedia "text/plain" "text/plain")

[<Fact>]
let ``matchMedia match any in server``() =
    Assert.Equal(Some "text/plain", matchMedia "*/*" "text/plain")

[<Fact>]
let ``matchMedia match any in client``() =
    Assert.Equal(Some "text/plain", matchMedia "text/plain" "*/*")

[<Fact>]
let ``Filter and sort media``() =
    let accept = "application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5" // actual Chrome Accepts header
    let serves = ["application/xml"; "text/xml"; "text/html"; "application/json"]
    let sorted = filterSortMedia serves accept
    Assert.Equal(["application/xml"; "text/html"; "text/xml"; "application/json"], sorted)

[<Fact>]
let ``Filter and sort media 2``() =
    let accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" // actual Firefox Accepts header
    let serves = ["text/html"; "application/json"; "application/xml"; "text/xml"]
    let sorted = filterSortMedia serves accept
    Assert.Equal(["text/html"; "application/xml"; "application/json"; "text/xml"], sorted)
