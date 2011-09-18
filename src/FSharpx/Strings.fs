namespace FSharpx

open System

module Strings =
    /// Checks wether the given text starts with the given prefix
    let startsWith prefix (text:string) = text.StartsWith prefix
