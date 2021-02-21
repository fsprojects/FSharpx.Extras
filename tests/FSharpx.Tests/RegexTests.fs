module FSharpx.Tests.RegexTests

open FSharpx.Text
open FSharpx
open NUnit.Framework
open System
open System.Text.RegularExpressions
open FsUnitTyped

let assertSome (x:'a option) : 'a =
    match x with
    | Some x -> x
    | None ->
        Assert.Fail "Expected Some, got None"
        failwith "impossible"

let runningOnMono = try System.Type.GetType("Mono.Runtime") <> null with e -> false 

[<Test>]
let ``tryMatch success``() =
    let m = "John Smith" |> Regex.tryMatch "(?i)^john .*" |> Option.map (fun m -> { m with MatchValue = m.MatchValue.ToLowerInvariant() })
    let m = assertSome m
    m.MatchValue |> shouldEqual "john smith"

[<Test>]
let ``replace with accumulator``() =
    let count, r = 
        Regex "%." 
        |> Regex.replaceWithAcc (fun s _ -> let s = s+1 in s, sprintf "@p%d" s) 0 "values (%d, %s)"
    count |> shouldEqual 2
    r |> shouldEqual "values (@p1, @p2)"

[<Test>]
let ``replace with fixed number of replacements``() =
    Regex "%."
    |> Regex.replaceWith ["@p1"; "@p2"] "values (%d, %s, %i)"
    |> shouldEqual "values (@p1, @p2, %i)"

let tryMatchName = Regex.tryMatchWithOptions RegexOptions.None "^(\w*) (\w* )?(\w*)$"

[<Test>]
let ``match with null input is none`` () =
    tryMatchName null |> Option.isNone |> shouldEqual true

[<Test>]
let ``non match is none`` () =
    tryMatchName "john" |> Option.isNone |> shouldEqual true

[<Test>]
let ``successful match with one optional group unsuccessful`` () =
    let result = tryMatchName "john smith"
    
    let result = assertSome result
    result.Groups.Length |> shouldEqual 3
    result.MatchValue |> shouldEqual "john smith"
    result.GroupValues |> shouldEqual ["john"; ""; "smith"]
    result.OptionalGroupValues |> shouldEqual [Some "john"; None; Some "smith"]

[<Test>]
let ``all variations yield same match value result`` () =
    let pattern = "^(\w*) (\w* )?(\w*)$"
    let input = "john smith"

    let withoutOptions = Regex.tryMatchWithOptions RegexOptions.None pattern input
    let withOptions = Regex.tryMatch pattern input
    
    let activeWithOptions = Regex.(|Match|_|) RegexOptions.None pattern input
    let activeCompiled = Regex.Compiled.(|Match|_|) pattern input
    let activeInterpreted = Regex.Interpreted.(|Match|_|) pattern input

    let withoutOptions =  assertSome withoutOptions
    withoutOptions.MatchValue |> shouldEqual input

    let withOptions =  assertSome withOptions
    withOptions.MatchValue |> shouldEqual input

    let activeWithOptions =  assertSome activeWithOptions
    activeWithOptions.MatchValue |> shouldEqual input

    let activeCompiled = assertSome activeCompiled
    activeCompiled.MatchValue |> shouldEqual input

    let activeInterpreted = assertSome activeInterpreted
    activeInterpreted.MatchValue |> shouldEqual input

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
    // See https://github.com/fsprojects/fsharpx/issues/302
    if not runningOnMono then 
        // example from http://msdn.microsoft.com/en-us/library/yd1hzczs.aspx#Invariant
        use __ = withCulture "tr-TR"
        let match' = Regex.tryMatch "(?i)FILE" "file"
        match'.IsSome |> shouldEqual true

[<Test>]
let ``Match active pattern is culture invariant``() =
    // See https://github.com/fsprojects/fsharpx/issues/302
    if not runningOnMono then 
        use __ = withCulture "tr-TR"
        match "file" with
        | Regex.Interpreted.Match "(?i)FILE" m -> m.MatchValue |> shouldEqual "file"
        | _ -> Assert.Fail ""
