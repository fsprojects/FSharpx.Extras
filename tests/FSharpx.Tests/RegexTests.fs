module FSharpx.Tests.RegexTests

open FSharpx
open NUnit.Framework
open System
open System.Text.RegularExpressions
open FsUnit

[<Test>]
let ``tryMatch success``() =
    let m = "John Smith" |> Regex.tryMatch "(?i)^john .*" |> Option.map (fun m -> { m with MatchValue = m.MatchValue.ToLowerInvariant() })
    m.IsSome |> should equal true
    m.Value.MatchValue |> should equal "john smith"

[<Test>]
let ``replace with accumulator``() =
    let count, r = 
        Regex "%." 
        |> Regex.replaceWithAcc (fun s _ -> let s = s+1 in s, sprintf "@p%d" s) 0 "values (%d, %s)"
    count |> should equal 2
    r |> should equal "values (@p1, @p2)"

[<Test>]
let ``replace with fixed number of replacements``() =
    Regex "%."
    |> Regex.replaceWith ["@p1"; "@p2"] "values (%d, %s, %i)"
    |> should equal "values (@p1, @p2, %i)"

let tryMatchName = Regex.tryMatchWithOptions RegexOptions.None "^(\w*) (\w* )?(\w*)$"

[<Test>]
let ``match with null input is none`` () =
    tryMatchName null |> Option.isNone |> should equal true

[<Test>]
let ``non match is none`` () =
    tryMatchName "john" |> Option.isNone |> should equal true

[<Test>]
let ``successful match with one optional group unsuccessful`` () =
    let result = tryMatchName "john smith"
    result.IsSome |> should equal true
    
    let result = result.Value

    result.Groups.Length |> should equal 3
    result.MatchValue |> should equal "john smith"
    result.GroupValues |> should equal ["john"; ""; "smith"]
    result.OptionalGroupValues |> should equal [Some "john"; None; Some "smith"]

[<Test>]
let ``all variations yield same match value result`` () =
    let pattern = "^(\w*) (\w* )?(\w*)$"
    let input = "john smith"

    let withoutOptions = Regex.tryMatchWithOptions RegexOptions.None pattern input
    let withOptions = Regex.tryMatch pattern input
    
    let activeWithOptions = Regex.(|Match|_|) RegexOptions.None pattern input
    let activeCompiled = Regex.Compiled.(|Match|_|) pattern input
    let activeInterpreted = Regex.Interpreted.(|Match|_|) pattern input

    withoutOptions.IsSome |> should equal true
    withoutOptions.Value.MatchValue |> should equal input

    withOptions.IsSome |> should equal true
    withOptions.Value.MatchValue |> should equal input

    activeWithOptions.IsSome |> should equal true
    activeWithOptions.Value.MatchValue |> should equal input

    activeCompiled.IsSome |> should equal true
    activeCompiled.Value.MatchValue |> should equal input

    activeInterpreted.IsSome |> should equal true
    activeInterpreted.Value.MatchValue |> should equal input

open System.Threading
open System.Globalization

let withCulture (c: string) =
    let old = Thread.CurrentThread.CurrentCulture
    Thread.CurrentThread.CurrentCulture <- CultureInfo c
    { new IDisposable with
        member x.Dispose() =
            Thread.CurrentThread.CurrentCulture <- old }

[<Test>]
let ``tryMatch is culture invariant``() =
    // example from http://msdn.microsoft.com/en-us/library/yd1hzczs.aspx#Invariant
    use __ = withCulture "tr-TR"
    let match' = Regex.tryMatch "(?i)FILE" "file"
    match'.IsSome |> should equal true

[<Test>]
let ``Match active pattern is culture invariant``() =
    use __ = withCulture "tr-TR"
    match "file" with
    | Regex.Interpreted.Match "(?i)FILE" m -> m.MatchValue |> should equal "file"
    | _ -> Assert.Fail ""
