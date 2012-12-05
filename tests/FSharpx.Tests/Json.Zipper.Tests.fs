module FSharpx.Tests.JSON.Zipper.Tests

open NUnit.Framework
open FSharpx.JSON
open FsUnit

type Context =
| Top
| Property of string * JsonValue

/// A zipper for JsonValues
type JSONZipper = { Focus : JsonValue; Path : Context } 

/// Returns the JsonValue under focus
let focus zipper = zipper.Focus     

/// Changes the element under the focus
let modifyText newText zipper = { zipper with Focus = JsonValue.String newText } 

/// Moves the zipper to a property
let toProperty name zipper = 
    match zipper.Focus with
    | JsonValue.Obj map -> { Focus = Map.find name map; Path = Property(name, zipper.Focus) }

/// Moves the zipper upwards
let up zipper = 
    match zipper.Path with
    | Top -> None
    | Property(name,parent) -> 
        match parent with
        | JsonValue.Obj map -> Some ({Focus = JsonValue.Obj(Map.add name zipper.Focus map); Path = Top } )

/// Moves the zipper to the top
let rec top zipper =
    match zipper.Path with
    | Top -> zipper
    | _ -> match up zipper with
           | None -> failwith "no top element"
           | Some x -> top x


/// Creates a Json zipper
let zipper jsonValue = { Focus = jsonValue; Path = Top }

/// Returns the whole Json document from the zipper
let getJson zipper = (top zipper).Focus 

[<Test>]
let ``Can modify a property in a simple document``() = 
    parse "{\"age\": 25,\"firstName\":\"John\",\"lastName\":\"Smith\"}"
    |> zipper 
    |> toProperty "firstName"
    |> modifyText "Johnny"
    |> getJson
    |> serialize 
    |> should equal "{\"age\":25,\"firstName\":\"Johnny\",\"lastName\":\"Smith\"}"