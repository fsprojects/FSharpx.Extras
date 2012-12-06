module FSharpx.Tests.JSON.Zipper.Tests

open NUnit.Framework
open FSharpx.JSON
open FSharpx.JSON.Zipper
open FsUnit

[<Test>]
let ``Can create zipper from an empty document``() = 
    parse "{ }"
    |> toZipper 
    |> fromZipper
    |> serialize 
    |> should equal "{}"

[<Test>]
let ``Can create zipper from an empty arry``() = 
    parse "[]"
    |> toZipper 
    |> fromZipper
    |> serialize 
    |> should equal "[]"

[<Test>]
let ``Can create zipper from a simple document``() = 
    parse "{\"id\": \"Open\"}"
    |> toZipper 
    |> fromZipper
    |> serialize 
    |> should equal "{\"id\":\"Open\"}"

[<Test>]
let ``Can create zipper from a simple array``() = 
    parse "[\"Test\",3]"
    |> toZipper 
    |> fromZipper
    |> serialize 
    |> should equal "[\"Test\",3]"

[<Test>]
let ``Can modify the first property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> toZipper 
    |> update (JsonValue.NumDecimal (decimal 26))
    |> fromZipper
    |> serialize 
    |> should equal "{\"age\":26,\"firstName\":\"John\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify a text property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> toZipper 
    |> toProperty "firstName"
    |> update (JsonValue.String "Johnny")
    |> fromZipper
    |> serialize 
    |> should equal "{\"age\":25,\"firstName\":\"Johnny\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify a bool property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\",\"isCool\":true}"
    |> toZipper 
    |> toProperty "isCool"
    |> update (JsonValue.Bool false)
    |> fromZipper
    |> serialize 
    |> should equal "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\",\"isCool\":false}"

[<Test>]
let ``Can modify a decimal property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> toZipper 
    |> toProperty "age"
    |> update (JsonValue.NumDecimal (decimal 26))
    |> fromZipper
    |> serialize 
    |> should equal "{\"age\":26,\"firstName\":\"John\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify a couple of properties in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> toZipper 
    |> toProperty "age"
    |> update (JsonValue.NumDecimal (decimal 26))
    |> toProperty "lastName"
    |> update (JsonValue.String "Forkmann")
    |> fromZipper
    |> serialize 
    |> should equal "{\"age\":26,\"firstName\":\"John\",\"lastName\":\"Forkmann\"}"

[<Test>]
let ``Can modify an array element a simple document``() = 
    parse "[1, 2, 3]"
    |> toZipper 
    |> right
    |> update (JsonValue.NumDecimal (decimal 26))
    |> fromZipper
    |> serialize 
    |> should equal "[1,26,3]"

[<Test>]
let ``Can insert an element to an array at the front``() = 
    parse "[1, 2, 3]"
    |> toZipper 
    |> insert (JsonValue.String "blub")
    |> fromZipper
    |> serialize 
    |> should equal "[\"blub\",1,2,3]"

[<Test>]
let ``Can insert an element to an array in the middle``() = 
    parse "[1, 2, 3]"
    |> toZipper 
    |> right
    |> right
    |> insert (JsonValue.String "bla")
    |> fromZipper
    |> serialize 
    |> should equal "[1,2,\"bla\",3]"

[<Test>]
let ``Can modify an array element a nested array``() = 
    parse "[ [\"Adam\", \"Eve\"], [\"Bonnie\", \"Clyde\"], [\"Donald\", \"Daisy\"], [\"Han\", \"Leia\"] ]" 
    |> toZipper 
    |> right
    |> down
    |> update (JsonValue.String "Steffen")
    |> fromZipper
    |> serialize 
    |> should equal "[[\"Adam\",\"Eve\"],[\"Steffen\",\"Clyde\"],[\"Donald\",\"Daisy\"],[\"Han\",\"Leia\"]]" 
 
[<Test>]
let ``Can modify a property inside a nested document``() = 
    parse "[{\"id\": \"Open\"}, null, {\"id\": \"Pause\"}]"
    |> toZipper 
    |> right
    |> right
    |> down
    |> toProperty "id"
    |> update (JsonValue.String "Stopped")
    |> fromZipper
    |> serialize 
    |> should equal "[{\"id\":\"Open\"},null,{\"id\":\"Stopped\"}]"

[<Test>]
let ``Can create a new object in a nested document``() = 
    parse "[{\"id\": \"Open\"}, null, {\"id\": \"Pause\"}]"
    |> toZipper 
    |> right
    |> update (JsonValue.Obj(["id",JsonValue.String "Tataa"]))
    |> fromZipper
    |> serialize 
    |> should equal "[{\"id\":\"Open\"},{\"id\":\"Tataa\"},{\"id\":\"Pause\"}]"


[<Test>]
let ``Can create update a text in a nested document``() = 
    parse "{\"a\":{\"id\": \"Open\"}}"
    |> toZipper 
    |> down
    |> update (JsonValue.String "Tataa")
    |> fromZipper
    |> serialize 
    |> should equal "{\"a\":{\"id\":\"Tataa\"}}"

[<Test>]
let ``Can create a new property in a simple document``() = 
    parse "{\"id\": \"Open\"}"
    |> toZipper 
    |> addProperty "id2" (JsonValue.String "Tataa")
    |> fromZipper
    |> serialize 
    |> should equal "{\"id\":\"Open\",\"id2\":\"Tataa\"}"

[<Test>]
let ``Can remove a property in a simple document``() = 
    parse "{\"id\": \"Open\",\"id2\":\"Tataa\",\"id3\":\"Tataa\"}"
    |> toZipper 
    |> toProperty "id2"
    |> remove
    |> fromZipper
    |> serialize 
    |> should equal "{\"id\":\"Open\",\"id3\":\"Tataa\"}"

[<Test>]
let ``Can remove the last in a simple document``() = 
    parse "{\"id\": \"Open\"}"
    |> toZipper
    |> remove
    |> fromZipper
    |> serialize 
    |> should equal "{}"
