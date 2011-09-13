namespace FSharpx

open System

module String =
  /// Checks wether the given text starts with the given prefix
  let startsWith prefix (text:string) = text.StartsWith prefix