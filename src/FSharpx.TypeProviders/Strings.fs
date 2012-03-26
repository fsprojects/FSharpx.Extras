namespace FSharpx

// TODO: Merge down to FSharpx.Core
open System

module Strings =
    // Active patterns & operators for parsing strings
    let (@?) (s:string) i = if i >= s.Length then None else Some s.[i]
    let sat f (c:option<char>) = match c with Some c when f c -> Some c | _ -> None
    let (|EOF|_|) c = match c with Some _ -> None | _ -> Some ()
    let (|LetterDigit|_|) = sat Char.IsLetterOrDigit
    let (|Upper|_|) = sat Char.IsUpper
    let (|Lower|_|) = sat Char.IsLower

    let pluralize(name:string) =
        if name.EndsWith("s") then name else
        name + "s"

    let singularize(name:string) =
        if name.EndsWith("s") then name.Substring(0,name.Length-1) else
        name