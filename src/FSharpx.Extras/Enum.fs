namespace FSharpx
open System.Linq
module Enum=
    [<CompiledName("TryParse")>]
    let tryParse s :'a option =
        match System.Enum.TryParse (s) with
        | true, v -> Some v
        | _ -> None
    [<CompiledName("Parse")>]
    let parse s : 'a =
        System.Enum.Parse(typeof<'a>, s) :?> 'a
    [<CompiledName("GetValues")>]
    let getValues<'t> ()= 
        let values = System.Enum.GetValues (typeof<'t>) 
        Enumerable.Cast<'t> values //Array.unbox