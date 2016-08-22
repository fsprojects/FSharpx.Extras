namespace FSharpx
open System.Linq

module Enum=
    let tryParse s :'a option when 'a:enum<'b> =
        match System.Enum.TryParse (s) with
        | true, v -> Some v
        | _ -> None
        
    let parse s : 'a =
        System.Enum.Parse(typeof<'a>, s) :?> 'a

    let getValues<'t> ()= 
        let values = System.Enum.GetValues (typeof<'t>) 
        Enumerable.Cast<'t> values //Array.unbox