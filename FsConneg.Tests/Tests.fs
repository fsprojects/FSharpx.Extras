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
let ``Best content type``() =
    let accept = "text/html; q=0.8; level=2, text/html; q=0.2; level=1, text/plain, image/jpeg"
    match bestContent "text" accept with
    | None -> failwith "no suitable content type found"
    | Some contentType -> Assert.Equal("text/plain", contentType)

