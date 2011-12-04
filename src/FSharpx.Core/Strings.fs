namespace FSharpx

open System

module Strings =
    /// Checks whether the given text starts with the given prefix
    let inline startsWith prefix (text:string) = text.StartsWith prefix

    /// Replaces the given "replacement" for every occurence of the pattern in the given text 
    let inline replace (pattern:string) replacement (text:string) = text.Replace(pattern,replacement)

    /// Returns a value indicating whether the specified substring occurs within this string
    let inline contains substr (t: string) = t.Contains substr
    
    /// Splits the given string at the given delimiter
    let inline split (delimiter:char) (text:string) = text.Split [|delimiter|]

    /// Converts a sequence of strings to a single string separated with the delimiters
    let inline separatedBy delimiter (items: string seq) = String.Join(delimiter, Array.ofSeq items)

    /// Returns if the string is null or empty
    let inline isNullOrEmpty text = String.IsNullOrEmpty text
