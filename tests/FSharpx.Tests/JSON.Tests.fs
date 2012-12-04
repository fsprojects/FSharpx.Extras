module FSharpx.Tests.JSON.ParserTests

open NUnit.Framework
open FSharpx.JSON
open FsUnit

[<Test>]
let ``Can parse empty document``() = 
    match parse "{}" with
    | JsonValue.Obj(map) when map = Map.empty -> ()
    | _ -> failwith "parse error"

[<Test>] 
let ``Can parse document with single property``() =
    match parse "{\"firstName\": \"John\"}" with
    | JsonValue.Obj(map) ->
        map |> Map.find "firstName" |> should equal (JsonValue.String "John")
    | _ -> failwith "parse error"

[<Test>] 
let ``Can parse document with text and integer``() =
    match parse "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"age\": 25}" with
    | JsonValue.Obj(map) ->
        map |> Map.find "firstName" |> should equal (JsonValue.String "John")
        map |> Map.find "lastName" |> should equal (JsonValue.String "Smith")
        map |> Map.find "age" |> should equal (JsonValue.NumDecimal (decimal 25))
    | _ -> failwith "parse error"

[<Test>] 
let ``Can parse document with text and float``() =
    match parse "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"age\": 25.25}" with
    | JsonValue.Obj(map) ->
        map |> Map.find "age" |> should equal (JsonValue.NumDecimal (decimal 25.25))
    | _ -> failwith "parse error"

[<Test>] 
let ``Can't parse document with no comma between properties ``() =    
    let exnRaised = ref false
    try
        parse "{ \"firstName\": \"Max\" \"lastName\": \"Mustermann\" \"age\": 26 \"isCool\": true }" |> ignore
    with 
    | _ -> exnRaised := true

    Assert.IsTrue(!exnRaised)

    
//[<Test>]
//let ``Can parse document with date``() =
//    let j = parse "{\"anniversary\": \"\\/Date(869080830450)\\/\"}"
//    j.GetDate "anniversary" |> should equal (new System.DateTime(1997, 07, 16, 19, 20, 30, 450, System.DateTimeKind.Utc))
//    (j.GetDate "anniversary").Kind |> should equal System.DateTimeKind.Utc
//
//[<Test>]
//let ``Can parse document with iso date``() =
//    let j = parse "{\"anniversary\": \"2009-05-19 14:39:22.500\"}"
//    j.GetDate "anniversary" |> should equal (new System.DateTime(2009, 05, 19, 14, 39, 22, 500, System.DateTimeKind.Local))
//    (j.GetDate "anniversary").Kind |> should equal System.DateTimeKind.Local
//
//[<Test>]
//[<SetCulture("zh-CN")>]
//let ``Can parse document with iso date in local culture``() =
//    let j = parse "{\"anniversary\": \"2009-05-19 14:39:22.500\"}"
//    j.GetDate "anniversary" |> should equal (new System.DateTime(2009, 05, 19, 14, 39, 22, 500, System.DateTimeKind.Local))
//    (j.GetDate "anniversary").Kind |> should equal System.DateTimeKind.Local
//
//[<Test>]
//let ``Can parse document with partial iso date``() =
//    let j = parse "{\"anniversary\": \"2009-05-19\"}"
//    j.GetDate "anniversary" |> should equal (new System.DateTime(2009, 05, 19, 0, 0, 0, System.DateTimeKind.Local))
//    (j.GetDate "anniversary").Kind |> should equal System.DateTimeKind.Local
//
//[<Test>]
//let ``Can parse document with timezone iso date``() =
//    let j = parse "{\"anniversary\": \"2009-05-19 14:39:22+0600\"}"
//    (j.GetDate "anniversary").ToUniversalTime() |> should equal (new System.DateTime(2009, 05, 19, 8, 39, 22, System.DateTimeKind.Utc))
//    
//[<Test>]
//let ``Can parse document with UTC iso date``() =
//    let j = parse "{\"anniversary\": \"2009-05-19 14:39:22Z\"}"
//    (j.GetDate "anniversary").ToUniversalTime() |> should equal (new System.DateTime(2009, 05, 19, 14, 39, 22, System.DateTimeKind.Utc))
//    (j.GetDate "anniversary").Kind |> should equal System.DateTimeKind.Utc
//
//[<Test>]    
//let ``Can parse document with timezone and fraction iso date``() =
//    let j = parse "{\"anniversary\": \"1997-07-16T19:20:30.45+01:00\"}"
//    (j.GetDate "anniversary").ToUniversalTime() |> should equal (new System.DateTime(1997, 07, 16, 18, 20, 30, 450, System.DateTimeKind.Utc))
//    
//// TODO: Due to limitations in the current ISO 8601 datetime parsing these fail, and should be made to pass
////[<Test>]
////let ``Cant Yet parse document with basic iso date``() =
////    let j = parse "{\"anniversary\": \"19810405\"}"
////    j.GetDate "anniversary" |> should equal (new System.DateTime(1981, 04, 05))
////
////[<Test>]
////let ``Cant Yet parse weird iso date``() =
////    let j = parse "{\"anniversary\": \"2010-02-18T16.5\"}"
////    j.GetDate "anniversary" |> should equal (new System.DateTime(2010, 02, 18, 16, 30, 00))
//
//[<Test>]
//let ``Can parse completely invalid, but close, date as string``() =
//    let j = parse "{\"anniversary\": \"2010-02-18T16.5:23.35:4\"}"
//    j.GetText "anniversary" |> should equal "2010-02-18T16.5:23.35:4"
//
open System.Globalization
open System.Threading

[<Test>] 
let ``Can parse document with fractional numbers``() =
    let originalCulture = Thread.CurrentThread.CurrentCulture
    Thread.CurrentThread.CurrentCulture <- new CultureInfo("pt-PT") // use a culture that uses ',' instead o '.' for decimal separators
    try 
        match parse "{ \"age\": 25.5}" with
        | JsonValue.Obj(map) ->
            map |> Map.find "age" |> should equal (JsonValue.NumDecimal (decimal 25.5))
        | _ -> failwith "parse error"        
    finally
        Thread.CurrentThread.CurrentCulture <- originalCulture

[<Test>]
let ``Can parse nested document`` () =
    match parse "{ \"main\": { \"title\": \"example\", \"nested\": { \"nestedTitle\": \"sub\" } } }" with
    | JsonValue.Obj(map) ->
        match  map |> Map.find "main" with
        | JsonValue.Obj(main) ->
            main |> Map.find "title" |> should equal (JsonValue.String "example")
            match  main |> Map.find "nested" with
            | JsonValue.Obj(nested) ->
                nested |> Map.find "nestedTitle" |> should equal (JsonValue.String "sub")
            | _ -> failwith "parse error"   
        | _ -> failwith "parse error"       
    | _ -> failwith "parse error"       
                
[<Test>] 
let ``Can parse document with booleans``() =
    match parse "{ \"hasTrue\": true, \"hasFalse\": false }" with
    | JsonValue.Obj(map) ->
        map |> Map.find "hasTrue" |> should equal (JsonValue.Bool true)
        map |> Map.find "hasFalse" |> should equal (JsonValue.Bool false)
    | _ -> failwith "parse error"

[<Test>] 
let ``Can parse document with null``() =    
    match parse "{ \"items\": [{\"id\": \"Open\"}, null, {\"id\": \"Pause\"}] }" with
    | JsonValue.Obj(map) ->
        match map |> Map.find "items" with
        | JsonValue.Array a ->
            a.[0] |> should equal (JsonValue.Obj(Map.add "id" (JsonValue.String "Open") Map.empty)) 
            a.[1] |> should equal JsonValue.Null 
            a.[2] |> should equal (JsonValue.Obj(Map.add "id" (JsonValue.String "Pause") Map.empty)) 
        | _ -> failwith "parse error"        
    | _ -> failwith "parse error"

[<Test>] 
let ``Can parse array in outermost scope``() =
    match parse "[{\"id\": \"Open\"}, null, {\"id\": \"Pause\"}]" with
    | JsonValue.Array a ->
        a.[0] |> should equal (JsonValue.Obj(Map.add "id" (JsonValue.String "Open") Map.empty)) 
        a.[1] |> should equal JsonValue.Null 
        a.[2] |> should equal (JsonValue.Obj(Map.add "id" (JsonValue.String "Pause") Map.empty)) 
    | _ -> failwith "parse error"        

[<Test>]
let ``Can parse a string from twitter api without throwing an error``() =
    let text =        
      "[{\"in_reply_to_status_id_str\":\"115445959386861568\",\"truncated\":false,\"in_reply_to_user_id_str\":\"40453522\",\"geo\":null,\"retweet_count\":0,\"contributors\":null,\"coordinates\":null,\"user\":{\"default_profile\":false,\"statuses_count\":3638,\"favourites_count\":28,\"protected\":false,\"profile_text_color\":\"634047\",\"profile_image_url\":\"http:\\/\\/a3.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\",\"name\":\"Steffen Forkmann\",\"profile_sidebar_fill_color\":\"E3E2DE\",\"listed_count\":46,\"following\":true,\"profile_background_tile\":false,\"utc_offset\":3600,\"description\":\"C#, F# and Dynamics NAV developer, blogger and sometimes speaker. Creator of FAKE - F# Make and NaturalSpec.\",\"location\":\"Hamburg \\/ Germany\",\"contributors_enabled\":false,\"verified\":false,\"profile_link_color\":\"088253\",\"followers_count\":471,\"url\":\"http:\\/\\/www.navision-blog.de\\/blog-mitglieder\\/steffen-forkmann-ueber-mich\\/\",\"profile_sidebar_border_color\":\"D3D2CF\",\"screen_name\":\"sforkmann\",\"default_profile_image\":false,\"notifications\":false,\"show_all_inline_media\":false,\"geo_enabled\":true,\"profile_use_background_image\":true,\"friends_count\":373,\"id_str\":\"22477880\",\"is_translator\":false,\"lang\":\"en\",\"time_zone\":\"Berlin\",\"created_at\":\"Mon Mar 02 12:04:39 +0000 2009\",\"profile_background_color\":\"EDECE9\",\"id\":22477880,\"follow_request_sent\":false,\"profile_background_image_url_https\":\"https:\\/\\/si0.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_background_image_url\":\"http:\\/\\/a1.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_image_url_https\":\"https:\\/\\/si0.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\"},\"favorited\":false,\"in_reply_to_screen_name\":\"ovatsus\",\"source\":\"\\u003Ca href=\\\"http:\\/\\/www.tweetdeck.com\\\" rel=\\\"nofollow\\\"\\u003ETweetDeck\\u003C\\/a\\u003E\",\"id_str\":\"115447331628916736\",\"in_reply_to_status_id\":115445959386861568,\"id\":115447331628916736,\"created_at\":\"Sun Sep 18 15:29:23 +0000 2011\",\"place\":null,\"retweeted\":false,\"in_reply_to_user_id\":40453522,\"text\":\"@ovatsus I know it's not complete. But I don't want to add a dependency on FParsec in #fsharpx. Can you send me samples where it fails?\"},{\"in_reply_to_status_id_str\":null,\"truncated\":false,\"in_reply_to_user_id_str\":null,\"geo\":null,\"retweet_count\":0,\"contributors\":null,\"coordinates\":null,\"user\":{\"statuses_count\":3637,\"favourites_count\":28,\"protected\":false,\"profile_text_color\":\"634047\",\"profile_image_url\":\"http:\\/\\/a3.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\",\"name\":\"Steffen Forkmann\",\"profile_sidebar_fill_color\":\"E3E2DE\",\"listed_count\":46,\"following\":true,\"profile_background_tile\":false,\"utc_offset\":3600,\"description\":\"C#, F# and Dynamics NAV developer, blogger and sometimes speaker. Creator of FAKE - F# Make and NaturalSpec.\",\"location\":\"Hamburg \\/ Germany\",\"contributors_enabled\":false,\"verified\":false,\"profile_link_color\":\"088253\",\"followers_count\":471,\"url\":\"http:\\/\\/www.navision-blog.de\\/blog-mitglieder\\/steffen-forkmann-ueber-mich\\/\",\"profile_sidebar_border_color\":\"D3D2CF\",\"screen_name\":\"sforkmann\",\"default_profile_image\":false,\"notifications\":false,\"show_all_inline_media\":false,\"geo_enabled\":true,\"profile_use_background_image\":true,\"friends_count\":372,\"id_str\":\"22477880\",\"is_translator\":false,\"lang\":\"en\",\"time_zone\":\"Berlin\",\"created_at\":\"Mon Mar 02 12:04:39 +0000 2009\",\"profile_background_color\":\"EDECE9\",\"id\":22477880,\"default_profile\":false,\"follow_request_sent\":false,\"profile_background_image_url_https\":\"https:\\/\\/si0.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_background_image_url\":\"http:\\/\\/a1.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_image_url_https\":\"https:\\/\\/si0.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\"},\"favorited\":false,\"in_reply_to_screen_name\":null,\"source\":\"\\u003Ca href=\\\"http:\\/\\/www.tweetdeck.com\\\" rel=\\\"nofollow\\\"\\u003ETweetDeck\\u003C\\/a\\u003E\",\"id_str\":\"115444490331889664\",\"in_reply_to_status_id\":null,\"id\":115444490331889664,\"created_at\":\"Sun Sep 18 15:18:06 +0000 2011\",\"possibly_sensitive\":false,\"place\":null,\"retweeted\":false,\"in_reply_to_user_id\":null,\"text\":\"Added a simple JSON parser to #fsharpx http:\\/\\/t.co\\/3JGI56SM - #fsharp\"}]"
    parse text |> printfn "%A"

[<Test>]
let ``Can parse array of numbers``() = 
    match parse "[1, 2, 3]" with
    | JsonValue.Array a ->
        a.[0] |> should equal (JsonValue.NumDecimal (decimal 1)) 
        a.[1] |> should equal (JsonValue.NumDecimal (decimal 2))
        a.[2] |> should equal (JsonValue.NumDecimal (decimal 3))
    | _ -> failwith "parse error"

[<Test>]
let ``Can parse simple array``() = 
    match parse "[\"Adam\",\"Eve\",\"Bonnie\",\"Clyde\",\"Donald\",\"Daisy\",\"Han\",\"Leia\"]" with
    | JsonValue.Array a ->
        a.[0] |> should equal (JsonValue.String "Adam") 
        a.[1] |> should equal (JsonValue.String "Eve") 
        a.[2] |> should equal (JsonValue.String "Bonnie") 
        a.[3] |> should equal (JsonValue.String "Clyde")         
        a.[7] |> should equal (JsonValue.String "Leia")
    | _ -> failwith "parse error"  

[<Test>]
let ``Can parse nested array``() = 
    match parse "[ [\"Adam\", \"Eve\"], [\"Bonnie\", \"Clyde\"], [\"Donald\", \"Daisy\"], [\"Han\", \"Leia\"] ]" with
    | JsonValue.Array a ->
        match a.[0] with
        | JsonValue.Array b ->
            b.[0] |> should equal (JsonValue.String "Adam") 
            b.[1] |> should equal (JsonValue.String "Eve")             
        | _ -> failwith "parse error"  
        match a.[1] with
        | JsonValue.Array b ->
            b.[0] |> should equal (JsonValue.String "Bonnie") 
            b.[1] |> should equal (JsonValue.String "Clyde")             
        | _ -> failwith "parse error"  
    | _ -> failwith "parse error"  