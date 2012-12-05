module FSharpx.Tests.JSON.Zipper.Tests

open NUnit.Framework
open FSharpx.JSON
open FsUnit

/// A zipper for JsonValues
/// inspired by https://gist.github.com/868042 
type JsonZipper = { Focus : JsonValue; Parent : JsonZipper option; Lefts : JsonValue list; Rights : JsonValue list } 

/// Returns the JsonValue under focus
let focus zipper = zipper.Focus     

/// Changes the element under the focus
let modifyText newValue zipper = { zipper with Focus = JsonValue.String newValue } 

/// Changes the element under the focus
let modifyDecimal newValue zipper = { zipper with Focus = JsonValue.NumDecimal newValue } 

/// Changes the element under the focus
let modifyBool newValue zipper = { zipper with Focus = JsonValue.Bool newValue } 


let left zipper = 
    match zipper.Lefts with
    | x::xs -> { Focus = x ; Lefts = xs; Parent = zipper.Parent; Rights = zipper.Focus::zipper.Rights }

let right zipper = 
    match zipper.Rights with
    | x::xs -> { Focus = x ; Lefts = zipper.Focus::zipper.Lefts; Parent = zipper.Parent; Rights = xs }

let down zipper =
    match zipper.Focus with
    | JsonValue.Obj map ->
        match [for kv in map -> kv.Value] with
        | x::xs -> { Parent = Some zipper; Lefts = [] ; Focus = x; Rights = xs}
    | JsonValue.Array (x::xs) -> { Parent = Some zipper; Lefts = [] ; Focus = x; Rights = xs}

let downToProperty name zipper =
    match zipper.Focus with
    | JsonValue.Obj map ->
        let rec loop rest position =
            match rest with
            | x::xs -> if x = name then position else loop xs (right position)
        
        zipper |> down |> loop [for kv in map -> kv.Key]        

let replaceChildren jsonValue children =
    match jsonValue with
    | JsonValue.Array _ -> JsonValue.Array children
    | JsonValue.Obj map ->
        let keys = [ for kv in map -> kv.Key ]
        let zipped = List.zip keys children
        JsonValue.Obj (Map.ofList zipped)

/// Moves the zipper upwards
let up zipper =
    match zipper.Parent with
    | Some parent ->  { parent with Focus = replaceChildren parent.Focus ((List.rev zipper.Lefts) @ zipper.Focus :: zipper.Rights) }

/// Creates a Json zipper
let zipper jsonValue =  { Focus = jsonValue; Parent = None; Lefts = []; Rights = []}

/// Returns the whole Json document from the zipper
let rec getJson zipper = 
    match zipper.Parent with
    | None -> zipper.Focus
    | Some parent -> up zipper |> getJson


[<Test>]
let ``Can modify the first property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> zipper 
    |> down
    |> modifyDecimal (decimal 26)
    |> getJson
    |> serialize 
    |> should equal "{\"age\":26,\"firstName\":\"John\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify the first text property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> zipper 
    |> down
    |> right
    |> modifyText "Johnny"
    |> getJson
    |> serialize 
    |> should equal "{\"age\":25,\"firstName\":\"Johnny\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify a text property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> zipper 
    |> downToProperty "firstName"
    |> modifyText "Johnny"
    |> getJson
    |> serialize 
    |> should equal "{\"age\":25,\"firstName\":\"Johnny\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify a bool property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\",\"isCool\":true}"
    |> zipper 
    |> downToProperty "isCool"
    |> modifyBool false
    |> getJson
    |> serialize 
    |> should equal "{\"age\":25,\"firstName\":\"John\",\"isCool\":false,\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify a decimal property in a simple document``() = 
    parse "{\"age\":25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> zipper 
    |> downToProperty "age"
    |> modifyDecimal (decimal 26)
    |> getJson
    |> serialize 
    |> should equal "{\"age\":26,\"firstName\":\"John\",\"lastName\":\"Smith\"}"

[<Test>]
let ``Can modify an array element a simple document``() = 
    parse "[1, 2, 3]"
    |> zipper 
    |> down
    |> right
    |> modifyDecimal (decimal 26)
    |> getJson
    |> serialize 
    |> should equal "[1,26,3]"