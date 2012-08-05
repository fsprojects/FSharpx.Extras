module FSharpx.TypeProviders.Tests.JSON.ParserTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit

[<Test>]
let ``Can parse empty document``() = 
    let j = parse "{}"
    j.GetType() |> should equal typeof<JObject>
    (j :?> JObject).Properties.Keys.Count |> should equal 0

[<Test>] 
let ``Can parse document with single property``() =
    let j = parse """{"firstName": "John"}"""
    j.GetText "firstName" |> should equal "John"

[<Test>] 
let ``Can parse document with text and integer``() =
    let j = parse """{"firstName": "John", "lastName": "Smith", "age": 25}"""
    j.GetText "firstName" |> should equal "John"
    j.GetText "lastName" |> should equal "Smith"
    j.GetNumber "age"  |> should equal 25

[<Test>] 
let ``Can parse document with text and float``() =
    let j = parse """{"firstName": "John", "lastName": "Smith", "age": 25.25}"""
    j.GetNumber "age"  |> should equal 25.25

open System.Globalization
open System.Threading

[<Test>] 
let ``Can parse document with fractional numbers``() =
    let originalCulture = Thread.CurrentThread.CurrentCulture
    Thread.CurrentThread.CurrentCulture <- new CultureInfo("pt-PT") // use a culture that uses ',' instead o '.' for decimal separators
    try 
        let j = parse """{ "age": 25.5}"""
        j.GetNumber "age" |> should equal 25.5
    finally
        Thread.CurrentThread.CurrentCulture <- originalCulture

[<Test>]
let ``Can parse nested document`` () =
    let j =
        """{
            "main": {
                "title": "example",
                "nested": {
                    "nestedTitle": "sub"
                }
            }
        }"""
        |> parse
    let main = j.GetJObject "main"

    main.GetText "title" |> should equal "example"
    let nested = main.GetJObject "nested" 
    nested.GetText "nestedTitle" |> should equal "sub"
                
[<Test>] 
let ``Can parse document with booleans``() =
    let j = parse """{ "hasTrue": true, "hasFalse": false }"""
    j.GetBoolean "hasTrue" |> should equal true
    j.GetBoolean "hasFalse" |> should equal false


[<Test>] 
let ``Can parse document with null``() =    
    let j = parse """{"items": [{"id": "Open"}, null, {"id": "Pause"}] }"""
    let jArray = j.GetJArray "items"
    jArray.Elements.[0].GetText "id" |> should equal "Open"
    jArray.Elements.[1].GetType() |> should equal typeof<JSONNull>
    jArray.Elements.[2].GetText "id" |> should equal "Pause"

[<Test>] 
let ``Can parse array in outermost scope``() =
    let jArray = parse """[{"id": "Open"}, null, {"id": "Pause"}]""" :?> JArray

    jArray.Elements.[0].GetText "id" |> should equal "Open"
    jArray.Elements.[1].GetType() |> should equal typeof<JSONNull>
    jArray.Elements.[2].GetText "id" |> should equal "Pause"

[<Test>]
let ``Can parse a string from twitter api without throwing an error``() =
    let text =        
      "[{\"in_reply_to_status_id_str\":\"115445959386861568\",\"truncated\":false,\"in_reply_to_user_id_str\":\"40453522\",\"geo\":null,\"retweet_count\":0,\"contributors\":null,\"coordinates\":null,\"user\":{\"default_profile\":false,\"statuses_count\":3638,\"favourites_count\":28,\"protected\":false,\"profile_text_color\":\"634047\",\"profile_image_url\":\"http:\\/\\/a3.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\",\"name\":\"Steffen Forkmann\",\"profile_sidebar_fill_color\":\"E3E2DE\",\"listed_count\":46,\"following\":true,\"profile_background_tile\":false,\"utc_offset\":3600,\"description\":\"C#, F# and Dynamics NAV developer, blogger and sometimes speaker. Creator of FAKE - F# Make and NaturalSpec.\",\"location\":\"Hamburg \\/ Germany\",\"contributors_enabled\":false,\"verified\":false,\"profile_link_color\":\"088253\",\"followers_count\":471,\"url\":\"http:\\/\\/www.navision-blog.de\\/blog-mitglieder\\/steffen-forkmann-ueber-mich\\/\",\"profile_sidebar_border_color\":\"D3D2CF\",\"screen_name\":\"sforkmann\",\"default_profile_image\":false,\"notifications\":false,\"show_all_inline_media\":false,\"geo_enabled\":true,\"profile_use_background_image\":true,\"friends_count\":373,\"id_str\":\"22477880\",\"is_translator\":false,\"lang\":\"en\",\"time_zone\":\"Berlin\",\"created_at\":\"Mon Mar 02 12:04:39 +0000 2009\",\"profile_background_color\":\"EDECE9\",\"id\":22477880,\"follow_request_sent\":false,\"profile_background_image_url_https\":\"https:\\/\\/si0.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_background_image_url\":\"http:\\/\\/a1.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_image_url_https\":\"https:\\/\\/si0.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\"},\"favorited\":false,\"in_reply_to_screen_name\":\"ovatsus\",\"source\":\"\\u003Ca href=\\\"http:\\/\\/www.tweetdeck.com\\\" rel=\\\"nofollow\\\"\\u003ETweetDeck\\u003C\\/a\\u003E\",\"id_str\":\"115447331628916736\",\"in_reply_to_status_id\":115445959386861568,\"id\":115447331628916736,\"created_at\":\"Sun Sep 18 15:29:23 +0000 2011\",\"place\":null,\"retweeted\":false,\"in_reply_to_user_id\":40453522,\"text\":\"@ovatsus I know it's not complete. But I don't want to add a dependency on FParsec in #fsharpx. Can you send me samples where it fails?\"},{\"in_reply_to_status_id_str\":null,\"truncated\":false,\"in_reply_to_user_id_str\":null,\"geo\":null,\"retweet_count\":0,\"contributors\":null,\"coordinates\":null,\"user\":{\"statuses_count\":3637,\"favourites_count\":28,\"protected\":false,\"profile_text_color\":\"634047\",\"profile_image_url\":\"http:\\/\\/a3.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\",\"name\":\"Steffen Forkmann\",\"profile_sidebar_fill_color\":\"E3E2DE\",\"listed_count\":46,\"following\":true,\"profile_background_tile\":false,\"utc_offset\":3600,\"description\":\"C#, F# and Dynamics NAV developer, blogger and sometimes speaker. Creator of FAKE - F# Make and NaturalSpec.\",\"location\":\"Hamburg \\/ Germany\",\"contributors_enabled\":false,\"verified\":false,\"profile_link_color\":\"088253\",\"followers_count\":471,\"url\":\"http:\\/\\/www.navision-blog.de\\/blog-mitglieder\\/steffen-forkmann-ueber-mich\\/\",\"profile_sidebar_border_color\":\"D3D2CF\",\"screen_name\":\"sforkmann\",\"default_profile_image\":false,\"notifications\":false,\"show_all_inline_media\":false,\"geo_enabled\":true,\"profile_use_background_image\":true,\"friends_count\":372,\"id_str\":\"22477880\",\"is_translator\":false,\"lang\":\"en\",\"time_zone\":\"Berlin\",\"created_at\":\"Mon Mar 02 12:04:39 +0000 2009\",\"profile_background_color\":\"EDECE9\",\"id\":22477880,\"default_profile\":false,\"follow_request_sent\":false,\"profile_background_image_url_https\":\"https:\\/\\/si0.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_background_image_url\":\"http:\\/\\/a1.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_image_url_https\":\"https:\\/\\/si0.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\"},\"favorited\":false,\"in_reply_to_screen_name\":null,\"source\":\"\\u003Ca href=\\\"http:\\/\\/www.tweetdeck.com\\\" rel=\\\"nofollow\\\"\\u003ETweetDeck\\u003C\\/a\\u003E\",\"id_str\":\"115444490331889664\",\"in_reply_to_status_id\":null,\"id\":115444490331889664,\"created_at\":\"Sun Sep 18 15:18:06 +0000 2011\",\"possibly_sensitive\":false,\"place\":null,\"retweeted\":false,\"in_reply_to_user_id\":null,\"text\":\"Added a simple JSON parser to #fsharpx http:\\/\\/t.co\\/3JGI56SM - #fsharp\"}]"
    parse text |> printfn "%A"

[<Test>]
let ``Can parse array of numbers``() = 
    let j = parse "[1, 2, 3]"
    j.GetType() |> should equal typeof<JArray>
    let j = j :?> JArray
    j.Elements.[0] |> should equal (Number 1.)
    j.Elements.[1] |> should equal (Number 2.)
    j.Elements.[2] |> should equal (Number 3.)

[<Test>]
let ``Quotes in strings are property escaped``() = 
    let jsonStr = "{\"short_description\":\"This a string with \\\"quotes\\\"\"}"
    let j = parse jsonStr
    j.ToString() |> should equal jsonStr
