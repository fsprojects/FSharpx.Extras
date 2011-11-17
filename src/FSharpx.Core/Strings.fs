namespace FSharpx

open System

module Strings =
    /// Checks whether the given text starts with the given prefix
    let startsWith prefix (text:string) = text.StartsWith prefix

    /// Returns a value indicating whether the specified substring occurs within this string
    let contains substr (t: string) = t.Contains substr
