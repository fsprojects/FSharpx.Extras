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

    /// From http://www.fssnip.net/4V/title/Check-if-value-is-a-valid-enum-or-flags-combination
    [<CompiledName("IsDefined")>]
    let isDefined<'a, 'b when 'a : enum<'b>> (value:'a) =
        let (!<) = box >> unbox >> uint64
        let typ = typeof<'a>
        if typ.IsDefined(typeof<System.FlagsAttribute>, false)
            then (!< value, System.Enum.GetValues(typ) |> unbox)
                 ||> Array.fold (fun n v -> n &&& ~~~(!< v)) = 0UL
            else System.Enum.IsDefined(typ, value)