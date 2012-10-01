module FSharpx.Tests.JSON.ParserTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit
open FSharpx.JSON.DocumentExtensions

[<Test>]
let ``Can parse empty document``() = 
    let j = parse "{}"
    j.GetType() |> should equal typeof<JObject>
    (j :?> JObject).Properties.Keys.Count |> should equal 0

[<Test>] 
let ``Can parse document with single property``() =
    let j = parse "{\"firstName\": \"John\"}"
    j.GetText "firstName" |> should equal "John"

[<Test>] 
let ``Can parse document with text and integer``() =
    let j = parse "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"age\": 25}"
    j.GetText "firstName" |> should equal "John"
    j.GetText "lastName" |> should equal "Smith"
    j.GetNumber "age"  |> should equal 25

[<Test>] 
let ``Can parse document with text and float``() =
    let j = parse "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"age\": 25.25}"
    j.GetNumber "age"  |> should equal 25.25

[<Test>]
let ``Can parse document with date``() =
    let j = parse "{\"anniversary\": \"\\/Date(869080830450)\\/\"}"
    j.GetDate "anniversary" |> should equal (new System.DateTime(1997, 07, 16, 19, 20, 30, 450))

[<Test>]
let ``Can parse document with iso date``() =
    let j = parse "{\"anniversary\": \"2009-05-19 14:39:22.500\"}"
    j.GetDate "anniversary" |> should equal (new System.DateTime(2009, 05, 19, 14, 39, 22, 500))

[<Test>]
let ``Can parse document with partial iso date``() =
    let j = parse "{\"anniversary\": \"2009-05-19\"}"
    j.GetDate "anniversary" |> should equal (new System.DateTime(2009, 05, 19))

[<Test>]
let ``Can parse document with timezone iso date``() =
    let j = parse "{\"anniversary\": \"2009-05-19 14:39:22+0600\"}"
    (j.GetDate "anniversary").ToUniversalTime() |> should equal (new System.DateTime(2009, 05, 19, 8, 39, 22))

// TODO: Due to limitations in the current ISO 8601 datetime parsing these fail, and should be made to pass
//[<Test>]
//let ``Cant Yet parse document with basic iso date``() =
//    let j = parse "{\"anniversary\": \"19810405\"}"
//    j.GetDate "anniversary" |> should equal (new System.DateTime(1981, 04, 05))
//
//[<Test>]
//let ``Cant Yet parse weird iso date``() =
//    let j = parse "{\"anniversary\": \"2010-02-18T16.5\"}"
//    j.GetDate "anniversary" |> should equal (new System.DateTime(2010, 02, 18, 16, 30, 00))

[<Test>]
let ``Can parse completely invalid, but close, date as string``() =
    let j = parse "{\"anniversary\": \"2010-02-18T16.5:23.35:4\"}"
    j.GetText "anniversary" |> should equal "2010-02-18T16.5:23.35:4"

open System.Globalization
open System.Threading

[<Test>] 
let ``Can parse document with fractional numbers``() =
    let originalCulture = Thread.CurrentThread.CurrentCulture
    Thread.CurrentThread.CurrentCulture <- new CultureInfo("pt-PT") // use a culture that uses ',' instead o '.' for decimal separators
    try 
        let j = parse "{ \"age\": 25.5}"
        j.GetNumber "age" |> should equal 25.5
    finally
        Thread.CurrentThread.CurrentCulture <- originalCulture

[<Test>]
let ``Can parse nested document`` () =
    let j = parse "{ \"main\": { \"title\": \"example\", \"nested\": { \"nestedTitle\": \"sub\" } } }"
    let main = j.GetJObject "main"

    main.GetText "title" |> should equal "example"
    let nested = main.GetJObject "nested" 
    nested.GetText "nestedTitle" |> should equal "sub"
                
[<Test>] 
let ``Can parse document with booleans``() =
    let j = parse "{ \"hasTrue\": true, \"hasFalse\": false }"
    j.GetBoolean "hasTrue" |> should equal true
    j.GetBoolean "hasFalse" |> should equal false


[<Test>] 
let ``Can parse document with null``() =    
    let j = parse "{ \"items\": [{\"id\": \"Open\"}, null, {\"id\": \"Pause\"}] }"
    let jArray = j.GetJArray "items"
    jArray.Elements.[0].GetText "id" |> should equal "Open"
    jArray.Elements.[1].GetType() |> should equal typeof<JSONNull>
    jArray.Elements.[2].GetText "id" |> should equal "Pause"

[<Test>] 
let ``Can parse array in outermost scope``() =
    let jArray = parse "[{\"id\": \"Open\"}, null, {\"id\": \"Pause\"}]" :?> JArray

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

[<Test>]
let ``Can parse simple array``() = 
    let j = parse "[\"Adam\",\"Eve\",\"Bonnie\",\"Clyde\",\"Donald\",\"Daisy\",\"Han\",\"Leia\"]"
    j.GetType() |> should equal typeof<JArray>
    let j x = (j :?> JArray).Elements.[x]
    j 0  |> should equal (Text "Adam")
    j 1 |> should equal (Text "Eve")
    j 2 |> should equal (Text "Bonnie")
    j 3 |> should equal (Text "Clyde")

[<Test>]
let ``Can parse nested array``() = 
    let j = parse "[ [\"Adam\", \"Eve\"], [\"Bonnie\", \"Clyde\"], [\"Donald\", \"Daisy\"], [\"Han\", \"Leia\"] ]"
    j.GetType() |> should equal typeof<JArray>
    let j x y = ((j :?> JArray).Elements.[x] :?> JArray).Elements.[y]
    j 0 0 |> should equal (Text "Adam")
    j 0 1 |> should equal (Text "Eve")
    j 1 0 |> should equal (Text "Bonnie")
    j 1 1 |> should equal (Text "Clyde")