namespace FSharpx

open System.Text.RegularExpressions

//no reason to have this within the Regex module i think
type ActiveMatch =
    {   Match: Match
        MatchValue: string
        Groups: Group list
        OptionalGroups: (Group option) list
        GroupValues: string list
        OptionalGroupValues: (string option) list }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]//need this so doesn't hide Regex class in C# assemblies (should consider for other extension modules as well)
module Regex =
    type Options = RegexOptions

    let replaceWithAcc folder state input (rx: Regex) =
        let acc = ref state
        let evaluator (m: Match) =
            let newState, result = folder !acc m
            acc := newState
            result
        let replacement: string = rx.Replace(input, evaluator)
        !acc, replacement

    let replaceWith replacements input (rx: Regex) =
        let folder replacements (matx: Match) =
            match replacements with
            | [] -> [], matx.Value
            | x::xs -> xs, x
        replaceWithAcc folder replacements input rx |> snd

    //only problem with this method is that the method signature does not convey the order of "pattern" and "input"
    ///flags:RegexOptions -> pattern:string -> input:string -> Regex.ActiveMatch option
    let tryMatchWithOptions flags pattern input =
        match input with
        | null -> None //Regex.Match will throw with null input, we return None instead
        | _ ->
            //using the static Regex.Match takes advantage of Regex caching
            match Regex.Match(input, pattern, flags) with
            | m when m.Success -> 
                //n.b. the head value of m.Groups is the match itself, which we discard
                //n.b. if a group is optional and doesn't match, it's Value is ""
                let groups = [for x in m.Groups -> x].Tail
                let optionalGroups = groups |> List.map (fun x -> if x.Success then Some(x) else None)
                let groupValues = groups |> List.map (fun x -> x.Value)
                let optionalGroupValues = optionalGroups |> List.map (function None -> None | Some(x) -> Some(x.Value))

                Some({ Match=m
                       MatchValue=m.Value
                       Groups=groups
                       OptionalGroups=optionalGroups
                       GroupValues=groupValues
                       OptionalGroupValues=optionalGroupValues })
            | _ -> None

    let inline tryMatch pattern input = tryMatchWithOptions Options.CultureInvariant pattern input

    let inline (|Match|_|) options pattern input = tryMatchWithOptions options pattern input

    module Compiled =
        //note: if we need to support Silverlight and other reduced runtimes that don't support RegexOptions.Compiled,
        //then it would be nice for us to detect that and fall back on RegexOptions.None here (compiling is just an
        //optimization detail, doesn't change behavior of regex otherwise, so doing this fall back allows library
        //users to share code between their full vs. silverlight applications more easily).
        let (|Match|_|) = (|Match|_|) (Options.Compiled ||| Options.CultureInvariant)

    module Interpreted =
        let (|Match|_|) = (|Match|_|) Options.CultureInvariant
