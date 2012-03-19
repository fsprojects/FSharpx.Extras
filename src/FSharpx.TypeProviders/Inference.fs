module FSharpx.TypeProviders.Inference

open System
open System.Xml.Linq
open FSharpx.TypeProviders.DSL
open System.Collections.Generic
open System.Globalization

/// Checks whether the string is a boolean value
let isBool (s:string) =
    let l = s.ToLower()
    l = "true" || l = "false" || l = "yes" || l = "no"

/// Checks whether the string is an int
let isInt (s:string) = Int32.TryParse s |> fst

/// Checks whether the string is a float
let isFloat (s:string) =
      Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) 
        |> fst

/// Checks whether all values of the sequence can be inferred to a special type
let inferType values =     
    if Seq.forall isBool values then typeof<bool>
    elif Seq.forall isInt values then typeof<int>
    elif Seq.forall isFloat values then typeof<float>
    else typeof<string>