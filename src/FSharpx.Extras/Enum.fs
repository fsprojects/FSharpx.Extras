module FSharpx.Enum
open System

[<CompiledName("TryParse")>]
let tryParse<'a when 'a :> Enum and 'a : ( new : unit -> 'a ) and 'a:struct > s :'a option =
    s |> Option.fromTryPattern Enum.TryParse

[<CompiledName("Parse")>]
let parse<'a when 'a :> Enum > s : 'a =
    Enum.Parse(typeof<'a>, s) :?> 'a

[<CompiledName("GetValues")>]
let getValues<'t when 't :> Enum > ()= 
    let values = Enum.GetValues (typeof<'t>)
    Seq.cast<'t> values

/// From http://www.fssnip.net/4V/title/Check-if-value-is-a-valid-enum-or-flags-combination
[<CompiledName("IsDefined")>]
let isDefined<'a when 'a :> Enum> (value:'a) : bool =
    let (!<) = box >> unbox >> uint64
    let typ = typeof<'a>
    if typ.IsDefined(typeof<FlagsAttribute>, false) then
        let negatedValidEnumValuesAsNumbers = getValues<'a>() |> Seq.map ((!<) >> (~~~))
        let testedValueAsNumber = !< value
        let reminder = 
            (testedValueAsNumber, negatedValidEnumValuesAsNumbers)
            ||> Seq.fold (&&&)
        reminder = 0UL
    else
        Enum.IsDefined(typ, value)

/// Returns the name of the enum as string
[<CompiledName("ToString")>]
let toString (enum:Enum) : string = enum.ToString()