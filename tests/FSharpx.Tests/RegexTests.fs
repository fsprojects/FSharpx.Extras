module FSharpx.Tests.RegexTests

open FSharpx
open NUnit.Framework
open Swensen.Unquote.Assertions
open System
open System.Text.RegularExpressions

[<Test>]
let ``tryMatch success``() =
    let m = "John Smith" |> Regex.tryMatch "(?i)^john .*" |> Option.map (fun m -> { m with MatchValue = m.MatchValue.ToLowerInvariant() })
    test <@ m.IsSome @>
    test <@ m.Value.MatchValue = "john smith" @>

[<Test>]
let ``replace with accumulator``() =
    let count, r = 
        Regex "%." 
        |> Regex.replaceWithAcc (fun s _ -> let s = s+1 in s, sprintf "@p%d" s) 0 "values (%d, %s)"
    test <@ count = 2 && r = "values (@p1, @p2)" @>

[<Test>]
let ``replace with fixed number of replacements``() =
    let r =
        Regex "%."
        |> Regex.replaceWith ["@p1"; "@p2"] "values (%d, %s, %i)"
    test <@ r = "values (@p1, @p2, %i)" @>

let tryMatchName = Regex.tryMatchWithOptions RegexOptions.None "^(\w*) (\w* )?(\w*)$"

[<Test>]
let ``match with null input is none`` () =
    test <@ tryMatchName null |> Option.isNone @>

[<Test>]
let ``non match is none`` () =
    test <@ tryMatchName "john" |> Option.isNone @>

[<Test>]
let ``successful match with one optional group unsuccessful`` () =
    let result = tryMatchName "john smith"
    test <@ result.IsSome @>
    
    let result = result.Value

    test <@ result.Match <> null @>
    test <@ result.Groups.Length = 3 @>
    test <@ result.MatchValue = "john smith" @>
    test <@ result.GroupValues = ["john"; ""; "smith"] @>
    test <@ result.OptionalGroupValues = [Some "john"; None; Some "smith"] @>


[<Test>]
let ``all variations yield same match value result`` () =
    let pattern = "^(\w*) (\w* )?(\w*)$"
    let input = "john smith"

    let withoutOptions = Regex.tryMatchWithOptions RegexOptions.None pattern input
    let withOptions = Regex.tryMatch pattern input
    
    let activeWithOptions = Regex.(|Match|_|) RegexOptions.None pattern input
    let activeCompiled = Regex.Compiled.(|Match|_|) pattern input
    let activeInterpreted = Regex.Interpreted.(|Match|_|) pattern input

    test <@ withoutOptions.IsSome @>
    test <@ withoutOptions.Value.MatchValue = input @>

    test <@ withOptions.IsSome @>
    test <@ withOptions.Value.MatchValue = input @>

    test <@ activeWithOptions.IsSome @>
    test <@ activeWithOptions.Value.MatchValue = input @>

    test <@ activeCompiled.IsSome @>
    test <@ activeCompiled.Value.MatchValue = input @>

    test <@ activeInterpreted.IsSome @>
    test <@ activeInterpreted.Value.MatchValue = input @>